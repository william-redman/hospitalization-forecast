# Load necessary libraries
library(lubridate)
library(dplyr)
library(readr)
library(MMWRweek)
library(tidyverse)

# Load the dataset and process it efficiently
df_hhs <- read_csv('target-data/season_2024_2025/hospitalization-data.csv') %>%
  mutate(date = as_date(time)) %>%
  select(-1) %>%  # Remove first column if not needed
  arrange(date) %>%
  mutate(mmwr_week = MMWRweek(date)$MMWRweek)

# Optimized WIS Calculation Function
WIS <- function(df_hhs, model1, model, date, forecast_date) {
  print(as.character(date))
  filename <- paste0("model-output/", model1, "/", date, "-", model, ".csv")
  print(filename)
  if (!file.exists(filename)){
    print('File path is null')
    return(NULL)  # Skip if file doesn't exist
  }
  
  forecast <- read_csv(filename, show_col_types = FALSE)
  print(forecast)
  
  if (nrow(forecast) == 0) return(NULL)  # Skip if file is empty
  
  # Ensure that location column is available
  if (!"location" %in% colnames(forecast)) {
    stop("Error: 'location' column not found in forecast data.")
  }
  
  state_vector <- c("Ontario","North East", "West", "East","Central","North West","Toronto")
  quantiles_vector <- c(0.025, 0.1, 0.25)
  
  df_WIS <- lapply(state_vector, function(state) {
    single_forecast <- forecast %>%
      filter(target_end_date == forecast_date, location == state)
    
    if (nrow(single_forecast) == 0) return(NULL)
    
    single_true <- df_hhs %>%
      filter(date == forecast_date, geo_value == state) %>%
      pull(value)
    
    if (length(single_true) == 0) return(NULL)
    
    median_forecast <- single_forecast %>%
      filter(output_type_id == 0.500) %>%
      pull(value)
    
    if (length(median_forecast) == 0) return(NULL)
    
    AE <- abs(single_true - median_forecast)
    MSE <- (single_true - median_forecast)^2
    WIS <- AE / 2
    
    # Vectorized quantile operations
    sapply(quantiles_vector, function(quantile) {
      lower <- single_forecast %>% filter(output_type_id == quantile) %>% pull(value)
      upper <- single_forecast %>% filter(output_type_id == 1 - quantile) %>% pull(value)
      
      if (length(lower) == 0 || length(upper) == 0) return(NULL)
      
      WIS <<- WIS + (quantile * (upper - lower) + 
                       (single_true < lower) * (lower - single_true) + 
                       (single_true > upper) * (single_true - upper))
    })
    
    WIS <- WIS / (length(quantiles_vector) + 0.5)
    
    return(data.frame(location = state, WIS = WIS, AE = AE, MSE = MSE))
  }) %>% bind_rows()
  
  if (is.null(df_WIS)) return(NULL)
  
  WIS_results <- data.frame(
    forecast_date = date,
    horizon = forecast_date,
    model = model1,
    WIS = mean(df_WIS$WIS, na.rm = TRUE),
    MAE = mean(df_WIS$AE, na.rm = TRUE),
    MSE = mean(df_WIS$MSE, na.rm = TRUE)
  )
  
  return(WIS_results)
}


# Define model names and date range
#model_metadata_directory <- 'model-metadata/'
#model_names <- tools::file_path_sans_ext(list.files(model_metadata_directory, pattern = "\\.(yaml|yml)$"))
model_output_dir <- "model-output"
model_names <- list.dirs(model_output_dir, full.names = FALSE, recursive = FALSE)

earliest_forecast_from_date <- as_date("2024-10-19")
most_recent_date <- Sys.Date()
forecast_from_date_vector <- seq(earliest_forecast_from_date, most_recent_date, by = 7)
forecast_from_date_vector <- "2024-10-26"

# Initialize WIS_all
WIS_all <- NULL

# Main Loop for Forecast Calculation
for (model_date in forecast_from_date_vector) {
  for (j in 0:3) {  # Forecast horizons (0 to 3 weeks)
    forecast_date <- as.Date(model_date) + (j * 7)
    for (model_name in model_names) {
      print(model_name)
      WIS_current <- WIS(df_hhs = df_hhs, model1 = model_name, model = model_name, date = model_date, forecast_date = forecast_date)
      if (!is.null(WIS_current)) {
        WIS_all <- bind_rows(WIS_all, WIS_current)
      }else{
        #print('File is Null')
      }
    }
  }
}

# Calculate WIS averages for each model and horizon
WIS_average <- expand.grid(Horizon = 0:3, Model = model_names) %>%
  mutate(Average_WIS = NA, Average_MAE = NA)

for (model_name in model_names) {
  for (h in 0:3) {
    WIS_horizon <- WIS_all %>% filter(model == model_name, horizon == (forecast_date + h * 7))
    WIS_average$Average_WIS[WIS_average$Model == model_name & WIS_average$Horizon == h] <- mean(WIS_horizon$WIS, na.rm = TRUE)
    WIS_average$Average_MAE[WIS_average$Model == model_name & WIS_average$Horizon == h] <- mean(WIS_horizon$MAE, na.rm = TRUE)
  }
}

# Save WIS results to CSV files
write_csv(WIS_average, "auxiliary-data/WIS_average.csv")
write_csv(WIS_all, "auxiliary-data/all_scores.csv")

# Model Output Aggregation
model_output_dir <- "model-output"
model_directories <- list.dirs(model_output_dir, full.names = TRUE, recursive = TRUE)

# Initialize list to store concatenated data
all_model_data <- lapply(model_directories, function(model_dir) {
  model_name <- basename(model_dir)
  model_files <- list.files(model_dir, pattern = "*.csv", full.names = TRUE)
  
  do.call(rbind, lapply(model_files, function(model_file) {
    model_data <- read_csv(model_file, show_col_types = FALSE) %>%
      mutate(across(where(is.character), as.numeric),
             output_type_id = as.numeric(output_type_id),
             model = model_name)
    return(model_data)
  }))
})

# Concatenate all model data
concatenated_data <- bind_rows(all_model_data)
write_csv(concatenated_data, "auxiliary-data/concatenated_model_output.csv")

# Process completion message
message("All model CSVs concatenated and saved to concatenated_model_data.csv")