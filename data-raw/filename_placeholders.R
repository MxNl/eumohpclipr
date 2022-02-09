## code to prepare `filename_placeholders` dataset

filename_placeholders <- c(
  "region_name_spatcov",
  "abbreviation_measure",
  "hydrologic_order",
  "spatial_resolution"
)

usethis::use_data(filename_placeholders, overwrite = TRUE)
