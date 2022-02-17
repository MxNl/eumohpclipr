library(furrr)
devtools::install()
devtools::load_all()

eumohp_clipped <- eumohp_clip(here::here("..", "macro_mohp_feature_test", "macro_mohp_feature", "output_data"),
  countries = "germany",
  # custom_sf_polygon = test_custom_sf_polygon |> dplyr::summarise(),
  buffer = 1E4,
  eumohp_version = "v013.1.1"
)

plan(multisession, workers = ceiling(length(eumohp_clipped)/3))

# eumohp_clipped |>
#   magrittr::extract(c(1, 10, 19)) |>
#   eumohp_plot(downsample = 200)

eumohp_clipped |>
  eumohp_write(directory_output = here::here("..", "output_test"),
               parallel = TRUE)
