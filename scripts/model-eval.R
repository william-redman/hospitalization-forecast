library(lubridate)
library(dplyr)
library(readr)
library(MMWRweek)

# Load and process dataset 
df_hhs <- read_csv('target-data/season_2024_2025/hospitalization-data.csv') %>% #'hospitalization/hospitalization-forecast/target-data/season_2024_2025/hospitalization-data.csv'
  mutate(date = as_date(time, format = "%d-%m-%Y"),
         mmwr_week = MMWRweek(date)$MMWRweek) %>%
  arrange(date)

# Define parameters hospitalization/
model_output_dir <- "model-output" #"hospitalization/hospitalization-forecast/model-output"
model_names <- list.dirs(model_output_dir, full.names = FALSE, recursive = FALSE)
current_reference_date <- floor_date(Sys.Date(), unit = "week") + days(6)
start_reference_date <- as_date("2024-10-19")
all_ref_dates <- seq(start_reference_date, current_reference_date, by = "7 days")
region_vector <- c("Ontario","North East", "West", "East","Central","North West","Toronto")
target_vector <- c('wk inc covid hosp','wk inc flu hosp','wk inc rsv hosp')

# Initialize results container
WIS_all <- list()

WIS <- function(single_forecast, model, date, forecast_date, region, tid, j) {
  quantiles_vector <- c(0.025, 0.1, 0.25)
  
  single_true <- df_hhs %>%
    filter(time == as_date(forecast_date), geo_value == region) %>%
    pull(covid)
  
  if (length(single_true) == 0) {
    cat("No true value for region:", region, "on date:", forecast_date, "\n")
    return(NULL)
  }
  
  median_forecast <- single_forecast %>%
    filter(output_type_id == 0.5) %>%
    pull(value)
  
  # Check if median_forecast has a value
  if (length(median_forecast) == 0) {
    cat("No median forecast for region:", region, "on date:", forecast_date, "\n")
    return(NULL)
  }
  
  # Calculate error metrics
  AE <- abs(single_true - median_forecast)
  MSE <- (single_true - median_forecast)^2
  WIS <- AE / 2
  
  for (quantile in quantiles_vector) {
    lower <- single_forecast %>% filter(output_type_id == quantile) %>% pull(value)
    upper <- single_forecast %>% filter(output_type_id == 1 - quantile) %>% pull(value)
    
    # Check if both lower and upper quantiles exist
    if (length(lower) == 0 || length(upper) == 0) {
      cat("Missing quantile data for region:", region, "quantile:", quantile, "\n")
      next  # Move to the next quantile
    }
    
    WIS <- WIS + (quantile * (upper - lower) + 
                    (single_true < lower) * (lower - single_true) + 
                    (single_true > upper) * (single_true - upper))
  }
  
  WIS <- WIS / (length(quantiles_vector) + 0.5)
  
  # Return results as a data frame only if all calculations succeeded
  df_WIS <- data.frame(location = region, WIS = WIS, AE = AE, MSE = MSE)
  return(data.frame(
    reference_date = date,
    target_end_date = forecast_date,
    model = model,
    WIS = mean(df_WIS$WIS, na.rm = TRUE),
    MAE = mean(df_WIS$AE, na.rm = TRUE),
    MSE = mean(df_WIS$MSE, na.rm = TRUE),
    region = region,
    target = tid,
    horizon = j
  ))
}


# Main Loop for Forecast Calculation
for (reference_date in all_ref_dates){
  reference_date <- as_date(reference_date)
  for (model in model_names){
    filename <- paste0("model-output/", model, "/", reference_date, "-", model, ".csv")
    
    # Check if file exists
    if (!file.exists(filename)) {
      next  # Skip to the next model if the file doesn't exist
    }
    
    # Read forecast data once per model
    forecast <- read_csv(filename, show_col_types = FALSE)
    
    # Filter forecast data only once for location and target
    for (region in region_vector) {
      for (tid in target_vector) {
        # Filter forecast data for the current region and target
        filtered_forecast <- forecast %>%
          filter(location == region, target == tid)
        
        # If no data for this combination, skip
        if (nrow(filtered_forecast) == 0) {
          next
        }
        
        # Loop over forecast horizons (0 to 3 weeks)
        for (j in 0:3) {
          target_date <- as.Date(reference_date) + (j * 7)
          
          # Filter for current horizon
          horizon_forecast <- filtered_forecast %>%
            filter(horizon == j)
          
          # If no data for this horizon, skip
          if (nrow(horizon_forecast) == 0) {
            next
          }
          
          # Log the current process
          cat("Ref. Date:", as.character(reference_date), 
              "| Model:", model, 
              "| Target Date:", as.character(target_date), 
              "| Region:", region, 
              "| Target:", tid, "\n")
          
          # Call WIS function with filtered forecast
          WIS_current <- WIS(
            single_forecast = horizon_forecast, 
            model = model, 
            date = as.character(reference_date), 
            forecast_date = as.character(target_date), 
            region = region, 
            tid = tid,
            j=j
          )
          
          # If WIS was successfully calculated, append it to the results
          if (!is.null(WIS_current)) {
            WIS_all <- bind_rows(WIS_all, WIS_current)
          } else {
            cat('WIS calculation returned NULL for model:', model, 
                '| Region:', region, '| Target:', tid, '| Horizon:', j, "\n")
          }
        }
      }
    }
  }
}

WIS_average <- expand.grid(Horizon = 0:3, Model = model_names) %>%
  mutate(Average_WIS = NA, Average_MAE = NA, Average_MSE = NA)

for (model_name in model_names) {
  for (h in 0:3) {
    WIS_horizon <- WIS_all %>% filter(model == model_name, target_end_date == (as_date(reference_date) + (h * 7)))
    WIS_average$Average_WIS[WIS_average$Model == model_name & WIS_average$Horizon == h] <- mean(WIS_horizon$WIS, na.rm = TRUE)
    WIS_average$Average_MAE[WIS_average$Model == model_name & WIS_average$Horizon == h] <- mean(WIS_horizon$MAE, na.rm = TRUE)
    WIS_average$Average_MSE[WIS_average$Model == model_name & WIS_average$Horizon == h] <- mean(WIS_horizon$MSE, na.rm = TRUE)
  }
}

# Write results to CSV
write_csv(WIS_average, "hospitalization-output/WIS_average.csv")
write_csv(WIS_all, "hospitalization-output/all_scores.csv")

# Aggregate model output
all_model_data <- lapply(list.dirs(model_output_dir, full.names = TRUE, recursive = FALSE), function(model_dir) {
  model_name <- basename(model_dir)
  model_files <- list.files(model_dir, pattern = "\\.csv$", full.names = TRUE)
  
  do.call(rbind, lapply(model_files, function(file) {
    read_csv(file, show_col_types = FALSE) %>%
      mutate(model = model_name,
             reference_date = if_else(is.na(as_date(dmy(reference_date))),
                                      as_date(as.numeric(reference_date)),
                                      as_date(dmy(reference_date))),
             target_end_date = if_else(is.na(as_date(dmy(target_end_date))),
                                       as_date(as.numeric(target_end_date)),
                                       as_date(dmy(target_end_date)))
             
             )
  }))
})

# Combine and clean data
concatenated_data <- bind_rows(all_model_data) %>%
  mutate(reference_date = if_else(is.na(as_date(dmy(reference_date))),
                                  as_date(as.numeric(reference_date)),
                                  as_date(dmy(reference_date))),
         target_end_date = if_else(is.na(as_date(dmy(target_end_date))),
                                   as_date(as.numeric(target_end_date)),
                                   as_date(dmy(target_end_date)))) %>%
  # Drop rows where either reference_date or target_end_date is NA
  filter(!is.na(reference_date), !is.na(target_end_date))

write_csv(concatenated_data, "hospitalization-output/concatenated_model_output.csv")
