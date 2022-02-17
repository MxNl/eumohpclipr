## code to prepare `eumohp_covered_countries` dataset
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)

eea39_countries <-
  c(
    "Austria",
    "Belgium",
    "Bulgaria",
    "Croatia",
    "Cyprus",
    "Czech Rep.",
    "Denmark",
    "Estonia",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Iceland",
    "Ireland",
    "Italy",
    "Latvia",
    "Liechtenstein",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Netherlands",
    "Norway",
    "Poland",
    "Portugal",
    "Romania",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Sweden",
    "Switzerland",
    "Turkey",
    "Albania",
    "Bosnia and Herz.",
    "Kosovo",
    "Montenegro",
    "Macedonia",
    "Serbia",
    "United Kingdom"
  )

eumohp_coverage <- here::here("data-raw", "eumohp_coverage.rds") |>
  read_rds()

eumohp_covered_countries <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  filter(name %in% eea39_countries) |>
  st_cast("POLYGON") |>
  st_transform(st_crs(eumohp_coverage)) |>
  {\(x) filter(x, st_intersects(x, eumohp_coverage, sparse = FALSE) %>% apply(1, any))}() |>
  select(name, name_long, abbrev, postal, su_a3) |>
  group_by(name, name_long, abbrev, postal, su_a3) |>
  summarise(.groups = "drop") |>
  st_cast("MULTIPOLYGON")

usethis::use_data(eumohp_covered_countries, overwrite = TRUE)
