
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)

# Define file paths
hospital_bed_occupancy_file <- "./auxiliary-data/data.xlsx"
phu_region_mapping_file <- "./auxiliary-data/phu_region_mapping.csv"

# Read hospital bed occupancy and region mapping data
hospital_bed_occupancy <- read_excel(hospital_bed_occupancy_file,
                                     col_types = c("text", "text", "text", "numeric", "date", "date", "numeric", "numeric", "numeric"))

phu_region_mapping <- read.csv(phu_region_mapping_file)

# Process hospital bed occupancy data
hospital_bed_occupancy <- hospital_bed_occupancy %>%
  replace_na(list(`Public health unit` = "NA")) %>%
  filter(`Public health unit` != 'NA') %>%
  left_join(phu_region_mapping, by = c('Public health unit' = 'NAME_ENG')) %>%
  select(`Public health unit`, `Outcome`, `Surveillance week`, `Week end date`, 
         `Number`, `OH_Name`, `Population_2021`) %>%
  filter(Outcome %in% c("COVID-19 hospital bed occupancy (total)", 
                        "Influenza hospital bed occupancy (total)", 
                        "RSV hospital bed occupancy (total)")) %>%
  mutate(Number = ceiling(Number)) %>%
  pivot_wider(names_from = Outcome, values_from = Number) %>%
  mutate(geo_type = ifelse(OH_Name == "Ontario", "Province", "OH Region")) %>%
  select(`Surveillance week`, `Week end date`, `OH_Name`, `geo_type`, 
         `COVID-19 hospital bed occupancy (total)`, 
         `Influenza hospital bed occupancy (total)`, 
         `RSV hospital bed occupancy (total)`, `Population_2021`) %>%
  rename(week = `Surveillance week`, time = `Week end date`, 
         geo_value = `OH_Name`, 
         covid = `COVID-19 hospital bed occupancy (total)`, 
         flu = `Influenza hospital bed occupancy (total)`, 
         rsv = `RSV hospital bed occupancy (total)`, 
         population = `Population_2021`) %>%
  arrange(as.Date(time)) %>%
  mutate(year = year(time)) |>
  mutate(week = week(time)) |>
  group_by(time,geo_value,geo_type,year,week) |>
  summarise(
    covid = sum(covid),
    rsv = sum(rsv),
    flu = sum(flu),
  ) |>
  ungroup()

hospital_bed_occupancy$year <- as.integer(hospital_bed_occupancy$year)

# Define seasons and filter data
start_week <- 35
end_week <- 34
years <- 2019:2024

# Create target directory
target_dir <- "./target-data"
archive_dir <-"./auxiliary-data/target-data-archive"
  
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

if (!dir.exists(archive_dir)) {
  dir.create(archive_dir, recursive = TRUE)
}

# Handle the special case for the 2024-2025 season
if (!dir.exists(file.path(archive_dir, "season_2023_2024"))) {
  # Loop through the years and save all data
  for (start_year in years) {
    end_year <- start_year + 1
    print(year,start_year)
    filtered_data <- hospital_bed_occupancy %>%
      filter((year == start_year & week >= start_week) | 
               (year == end_year & week <= end_week))
   
    filtered_data <- filtered_data |>
      select(time, geo_value,	geo_type,	covid,	flu,	rsv)
    
    # Create directory and file path
    if (start_year == 2024){
      dir_name <- file.path(target_dir, paste0("season_", start_year, "_", end_year))
    } else{
      dir_name <- file.path(archive_dir, paste0("season_", start_year, "_", end_year))
    }
    
    if (!file.exists(dir_name)) {
      dir.create(dir_name, showWarnings = FALSE)
    }
    
    file_path <- file.path(dir_name, "hospitalization-data.csv")
    
    if (start_year<2022){
      filtered_data = filtered_data |>
        select(-rsv,-flu)
    }
    
    # Save filtered data to CSV
    write.csv(filtered_data, file = file_path, row.names = FALSE)
    cat("Data saved for season:", start_year, "-", end_year, "to", file_path, "\n")
  }
} else{
  start_year <- 2024
  end_year <- 2025
    filtered_data <- hospital_bed_occupancy %>%
      filter((year == start_year & week >= start_week) | 
               (year == end_year & week <= end_week))
    filtered_data <- filtered_data |>
      select(time, geo_value,	geo_type,	covid,	flu,	rsv)
  
    # Create directory and file path
    dir_name <- file.path(target_dir, paste0("season_", start_year, "_", end_year))
    if (!file.exists(dir_name)){
     dir.create(dir_name, showWarnings = FALSE)
    }
    file_path <- file.path(dir_name, "hospitalization-data.csv")
  
    # Save filtered data to CSV
    write.csv(filtered_data, file = file_path, row.names = FALSE)
    cat("Data saved for season:", start_year, "-", end_year, "to", file_path, "\n")
  }
