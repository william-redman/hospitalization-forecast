library(lubridate)
library(dplyr)
library(readr)
library(MMWRweek)

# Load and process dataset
df_hhs <- read_csv('hospitalization/target-data/season_2024_2025/hospitalization-data.csv') %>% #'hospitalization/hospitalization-forecast/target-data/season_2024_2025/hospitalization-data.csv'
  mutate(date = as_date(time, format = "%d-%m-%Y"),
         mmwr_week = MMWRweek(date)$MMWRweek) %>%
  arrange(date)

# Define parameters
model_output_dir <- "hospitalization/model-output" #"hospitalization/hospitalization-forecast/model-output"
model_names <- list.dirs(model_output_dir, full.names = FALSE, recursive = FALSE)
current_reference_date <- floor_date(Sys.Date(), unit = "week") + days(6)
start_reference_date <- as_date("2024-10-26")
all_ref_dates <- seq(start_reference_date, current_reference_date, by = "7 days")
region_vector <- c("Ontario","North East", "West", "East","Central","North West","Toronto")
target_vector <- c('wk inc covid hosp','wk inc flu hosp','wk inc rsv hosp')

# Initialize results container
WIS_all <- list()

# Define WIS function for scoring
WIS <- function(single_forecast, single_true) {
  AE <- abs(single_true - median(single_forecast$value[single_forecast$output_type_id == 0.5]))
  MSE <- AE^2
  quantiles <- c(0.025, 0.1, 0.25)
  
  WIS <- AE / 2 + sum(sapply(quantiles, function(q) {
    lower <- single_forecast$value[single_forecast$output_type_id == q]
    upper <- single_forecast$value[single_forecast$output_type_id == 1 - q]
    q * (upper - lower) + ifelse(single_true < lower, lower - single_true, 0) + ifelse(single_true > upper, single_true - upper, 0)
  }, USE.NAMES = FALSE))
  
  WIS / (length(quantiles) + 0.5)
}

# Main Loop for Forecast Calculation
for (reference_date in all_ref_dates) {
  for (model in model_names) {
    filename <- file.path(model_output_dir, model, paste0(reference_date, "-", model, ".csv"))
    if (!file.exists(filename)) next
    
    forecast <- read_csv(filename, show_col_types = FALSE)
    for (region in region_vector) {
      for (tid in target_vector) {
        filtered_forecast <- forecast %>%
          filter(location == region, target == tid)
        
        if (nrow(filtered_forecast) == 0) next
        
        for (j in 0:3) {
          target_date <- reference_date + days(7 * j)
          horizon_forecast <- filtered_forecast %>% filter(horizon == j)
          
          if (nrow(horizon_forecast) == 0) next
          
          single_true <- df_hhs %>% filter(time == target_date, geo_value == region) %>% pull(covid)
          if (length(single_true) == 0) next
          
          WIS_result <- WIS(horizon_forecast, single_true)
          
          # Append results to list
          WIS_all[[length(WIS_all) + 1]] <- data.frame(
            reference_date = reference_date,
            target_end_date = target_date,
            model = model,
            WIS = WIS_result,
            AE = abs(single_true - median(horizon_forecast$value[horizon_forecast$output_type_id == 0.5])),
            MSE = (single_true - median(horizon_forecast$value[horizon_forecast$output_type_id == 0.5]))^2,
            region = region,
            target = tid,
            horizon = j
          )
        }
      }
    }
  }
}

# Concatenate all results
WIS_all <- bind_rows(WIS_all)

# Calculate average scores by model and horizon
WIS_average <- expand.grid(Horizon = 0:3, Model = model_names) %>%
  rowwise() %>%
  mutate(
    Average_WIS = mean(WIS_all$WIS[WIS_all$model == Model & WIS_all$horizon == Horizon], na.rm = TRUE),
    Average_MAE = mean(WIS_all$AE[WIS_all$model == Model & WIS_all$horizon == Horizon], na.rm = TRUE),
    Average_MSE = mean(WIS_all$MSE[WIS_all$model == Model & WIS_all$horizon == Horizon], na.rm = TRUE)
  ) %>%
  ungroup()

# Write results to CSV
write_csv(WIS_average, "hospitalization_WIS_average.csv")
write_csv(WIS_all, "hospitalization_all_scores.csv")

# Aggregate model output
all_model_data <- lapply(list.dirs(model_output_dir, full.names = TRUE, recursive = FALSE), function(model_dir) {
  model_name <- basename(model_dir)
  model_files <- list.files(model_dir, pattern = "\\.csv$", full.names = TRUE)
  
  do.call(rbind, lapply(model_files, function(file) {
    read_csv(file, show_col_types = FALSE) %>%
      mutate(model = model_name)
  }))
})

# Combine and clean data
concatenated_data <- bind_rows(all_model_data) %>%
  mutate(
    reference_date = as_date(as.numeric(reference_date), origin = "1970-01-01"),
    target_end_date = as_date(as.numeric(target_end_date), origin = "1970-01-01")
  ) %>%
  filter(!is.na(reference_date), !is.na(target_end_date))

write_csv(concatenated_data, "hospitalization_concatenated_model_output.csv")
