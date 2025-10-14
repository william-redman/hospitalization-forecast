library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
library(readr)

# ---- File paths ----
hospital_bed_occupancy_file <- "./auxiliary-data/data.xlsx"
phu_region_mapping_file     <- "./auxiliary-data/phu_region_mapping.csv"

stopifnot(file.exists(hospital_bed_occupancy_file),
          file.exists(phu_region_mapping_file))

# ---- Read data ----
hospital_bed_occupancy_raw <- read_excel(hospital_bed_occupancy_file)

phu_region_mapping <- read_csv(phu_region_mapping_file, show_col_types = FALSE)

# ---- Process hospital bed occupancy ----
# Expected columns - "Public health unit", "Outcome", "Surveillance week", "Week end date", "Number", ...
hospital_bed_occupancy <- hospital_bed_occupancy_raw %>%
  replace_na(list(`Public health unit` = "NA")) %>%
  filter(`Public health unit` != "NA") %>%
  left_join(
    phu_region_mapping,
    by = c("Public health unit" = "NAME_ENG")
  ) %>%

  select(
    `Public health unit`, Outcome, `Surveillance week`, `Week end date`,
    Number, OH_Name, Population_2021
  ) %>%
  # filter the three outcomes of interest
  filter(Outcome %in% c(
    "COVID-19 hospital bed occupancy (total)",
    "Influenza hospital bed occupancy (total)",
    "RSV hospital bed occupancy (total)"
  )) %>%

  mutate(Number = ceiling(as.numeric(Number))) %>%
  pivot_wider(names_from = Outcome, values_from = Number) %>%
  mutate(
    geo_type = if_else(OH_Name == "Ontario", "Province", "OH Region")
  ) %>%
  select(
    `Surveillance week`, `Week end date`, OH_Name, geo_type,
    `COVID-19 hospital bed occupancy (total)`,
    `Influenza hospital bed occupancy (total)`,
    `RSV hospital bed occupancy (total)`,
    Population_2021
  ) %>%
  rename(
    week_reported = `Surveillance week`,
    time         = `Week end date`,
    geo_value    = OH_Name,
    covid        = `COVID-19 hospital bed occupancy (total)`,
    flu          = `Influenza hospital bed occupancy (total)`,
    rsv          = `RSV hospital bed occupancy (total)`,
    population   = Population_2021
  ) %>%
  arrange(as.Date(time)) %>%
  mutate(
    year = year(time),
    week = isoweek(time)  
  ) %>%
  group_by(time, geo_value, geo_type, year, week) %>%
  summarise(
    covid = sum(covid, na.rm = TRUE),
    rsv   = sum(rsv,   na.rm = TRUE),
    flu   = sum(flu,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.integer(year))

# ---- Season slicing params ----
start_week <- 35
end_week   <- 34
years      <- 2019:2025        # starting years of seasons to export
current_start_year <- 2025     # the active (non-archived) season

# ---- Target dirs ----
target_dir  <- "./target-data"
archive_dir <- "./auxiliary-data/target-data-archive"

if (!dir.exists(target_dir))  dir.create(target_dir,  recursive = TRUE)
if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)

# build season dir path (current season -> target, others -> archive)
season_dir <- function(start_year) {
  end_year <- start_year + 1
  base <- if (start_year == current_start_year) target_dir else archive_dir
  file.path(base, sprintf("season_%d_%d", start_year, end_year))
}

# ---- Export logic ----
# if archives are missing the most recent archived season, rebuild all; otherwise only export the current season.
most_recent_archived <- sprintf("season_%d_%d", 2024, 2025)
rebuild_all <- !dir.exists(file.path(archive_dir, most_recent_archived))

export_season <- function(start_year) {
  end_year <- start_year + 1
  
  # season filter: week 35..52/53 in start_year OR week 1..34 in end_year
  filtered_data <- hospital_bed_occupancy %>%
    filter((year == start_year & week >= start_week) |
             (year == end_year   & week <= end_week)) %>%
    select(time, geo_value, geo_type, covid, flu, rsv)
  
  # For pre-2022, drop flu/RSV if those series are not available/desired
  if (start_year < 2022) {
    filtered_data <- filtered_data %>% select(time, geo_value, geo_type, covid)
  }
  
  dir_name  <- season_dir(start_year)
  if (!dir.exists(dir_name)) dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  file_path <- file.path(dir_name, "hospitalization-data.csv")
  filtered_data <- filtered_data %>% 
    mutate(time = as.Date(time)) %>%
    arrange(desc(time))          
  write_csv(filtered_data, file_path)
  message(sprintf("Data saved for season: %d-%d -> %s", start_year, end_year, file_path))
}

if (rebuild_all) {
  purrr::walk(years, export_season)
} else {
  export_season(current_start_year)
}
