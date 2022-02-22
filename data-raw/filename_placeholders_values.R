## code to prepare `filename_placeholders_values` dataset

library(dplyr)
library(readr)

filename_placeholders_values <-
  read_csv(here::here("data-raw", "filename_table.csv")) |>
  janitor::clean_names() |>
  select(-x1, -row, -description) |>
  rename(
    filename_placeholders = placeholder_in_output_file_name,
    filename_placeholder_values = value_use_this_in_search_bar
  ) |>
  mutate(filename_placeholders = recode(filename_placeholders,
    "region name for spatial coverage" = "region_name_spatcov",
    "abbreviation of the EU-MOHP measure" = "abbreviation_measure",
    "hydrologic order" = "hydrologic_order",
    "spatial resolution" = "spatial_resolution"
  )) |>
  tibble::deframe()

usethis::use_data(filename_placeholders_values, overwrite = TRUE, internal = TRUE)
