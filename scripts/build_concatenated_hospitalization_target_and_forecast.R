library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(purrr)
withr::local_options(list(readr.show_col_types = FALSE))

# concatenate hospitalization data

seasons <- 2019:2025
archive_base <- "auxiliary-data/target-data-archive"
current_season_label <- "2025-2026"
current_season_path  <- file.path("target-data", "season_2025_2026", "hospitalization-data.csv")

# candidate file list (archive + current), then keep only existing paths
files_tbl <- bind_rows(
  tibble(
    start_year   = seasons,
    season_label = sprintf("%d-%d", seasons, seasons + 1),
    path         = file.path(archive_base, sprintf("season_%d_%d", seasons, seasons + 1), "hospitalization-data.csv")
  ),
  tibble(
    start_year   = 2025L,
    season_label = current_season_label,
    path         = current_season_path
  )
) %>%
  filter(file.exists(path)) %>%
  distinct(path, .keep_all = TRUE)

read_hosp <- function(path, season_label) {
  read_csv(
    path,
    col_types = cols(.default = col_guess(), time = col_date())
  ) %>%
    mutate(Season = season_label)
}

final_data <- if (nrow(files_tbl) == 0) {
  warning("No hospitalization-data.csv files found in archive or current season directories.")
  tibble()
} else {
  map2_dfr(files_tbl$path, files_tbl$season_label, read_hosp) %>%
    mutate(time = as_date(time)) %>%   # ensure plain Date
    arrange(desc(time))                # newest first
}

write_csv(final_data, "auxiliary-data/concatenated_hospitalization_data.csv")
message("Wrote: auxiliary-data/concatenated_hospitalization_data.csv")

# concatenate model outputs (robust date parsing, filename-date filter)

model_output_dir <- "model-output"

parse_maybe_date <- function(x) {
  suppressWarnings({
    num <- as.numeric(x)
    out <- as_date(num, origin = "1970-01-01")          
    need <- is.na(out)
    if (any(need)) {
      out[need] <- as_date(parse_date_time(
        x[need],
        orders = c("Ymd", "Y-m-d", "dmY", "d-m-Y", "Ymd HMS", "Y-m-d H:M:S"),
        tz = "UTC"
      ))
    }
    out
  })
}

process_model_file <- function(file) {
  model_name <- basename(dirname(file))
  df <- read_csv(file, progress = FALSE)
  
  df %>%
    mutate(
      model = model_name,
      reference_date  = if ("reference_date"  %in% names(.)) parse_maybe_date(reference_date)  else as_date(NA),
      target_end_date = if ("target_end_date" %in% names(.)) parse_maybe_date(target_end_date) else as_date(NA),
      value = pmax(0, ceiling(suppressWarnings(as.numeric(replace_na(value, 0)))))
    ) %>%
    filter(!is.na(reference_date), !is.na(target_end_date))
}

# keep only files whose basename starts with a date >= 2025-08-30 ---
extract_leading_date <- function(paths) {
  bases <- basename(paths)
  m <- regexpr("^\\d{4}-\\d{2}-\\d{2}", bases)
  # Pull the matched date string, or NA if no match
  ds <- ifelse(m == -1, NA_character_, regmatches(bases, m))
  ymd(ds)
}

cutoff_date <- as_date("2025-08-30")
all_csvs <- list.files(model_output_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

if (length(all_csvs) == 0) {
  message("No model CSV files found under: ", model_output_dir)
  all_model_data <- tibble()
} else {
  file_dates <- extract_leading_date(all_csvs)
  keep <- !is.na(file_dates) & file_dates >= cutoff_date
  model_files <- all_csvs[keep]
  
  all_model_data <- if (length(model_files) == 0) {
    message("No model CSV files on or after ", cutoff_date, " with a leading YYYY-MM-DD in the filename.")
    tibble()
  } else {
    map_dfr(model_files, process_model_file) %>%
      arrange(model, desc(reference_date), desc(target_end_date))
  }
}

all_model_data

if (nrow(all_model_data) > 0) {
  write_csv(all_model_data, "auxiliary-data/concatenated_model_output.csv")
  message("Wrote: auxiliary-data/concatenated_model_output.csv")
} else {
  message("No model data to write.")
  write_csv(all_model_data, "auxiliary-data/concatenated_model_output.csv")
}
