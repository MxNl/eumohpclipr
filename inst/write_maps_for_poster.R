library(magrittr)

spatial_extent <- filename_placeholders_values[names(filename_placeholders_values) == "region_name_spatcov"]
  # purrr::discard(stringr::str_detect, pattern = "turkey")
# spatial_extent <- "italy1"

path_to_eumohp <- here::here(
  "..",
  "macro_mohp_feature_test",
  "macro_mohp_feature",
  "output_data"
)

.eumohp_starsproxy <- eumohp_clip(path_to_eumohp,
                                  region_name_spatcov = spatial_extent,
                                  hydrologic_order = c(5),
                                  # abbreviation_measure = "dsd",
                                  eumohp_version = "v013.1.1"
)


.plot_single_order <- function(stars_object, name, outline, downsample = 50) {

  eumohp_measures <- filename_placeholders_values[
    names(filename_placeholders_values) == "abbreviation_measure"]
  eumohp_measure <- name |> stringr::word(start = 2, sep = "_")

  if (eumohp_measure == eumohp_measures[2]) {
    labels <- \ (x) {
      x / 1E4
    }
    unit_label <- "[ - ]"
  } else if (eumohp_measure %in% eumohp_measures[c(1, 3)]) {
    labels <- \ (x) {
      x / 1E3
    }
    unit_label <- "[km]"
  }

  ggplot2::ggplot() +
    stars::geom_stars(data = stars_object,
                      downsample = downsample) +
    # ggplot2::geom_sf(data = outline,
    #                  fill = NA,
    #                  colour = "white",
    #                  size = .5) +
    ggplot2::scale_fill_viridis_c(na.value = NA,
                                  labels = labels) +
    ggplot2::coord_sf() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

outline_sardignia <- sf::st_read("europe_oultine.shp")

.eumohp_starsproxy %>%
  # purrr::chuck(1) |>
  magrittr::extract(stringr::str_detect(names(.), "_lp_")) |>
  purrr::imap(.plot_single_order, outline = outline_sardignia, downsample = 100) |>
  # purrr::set_names("sdf") |>
  purrr::imap(~ ggplot2::ggsave(paste0(.y, ".png"), .x, device = "png"), scale = .5)

  eumohp_plot(downsample = 10)





single_plots |> names()
purrr::chuck(1) |> names()

patchwork::wrap_plots(nrow = 3) +
  patchwork::plot_annotation(
    title = .eumohp_starsproxy |>
      names() |>
      purrr::chuck(1) |>
      stringr::word(start = 1, sep = "_") |> {
        \ (x) stringr::str_c("eumohp", x, sep = " - ")
      }() |>
      stringr::str_to_title(),
    theme = ggplot2::theme(plot.title = ggplot2::element_text(
      hjust = .5,
      size = 12
    ))
  )
