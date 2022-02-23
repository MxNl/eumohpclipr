.plot_single_order <- function(stars_object, name, downsample = 50) {

  eumohp_measures <- filename_placeholders_values[
    names(filename_placeholders_values) == "abbreviation_measure"]
  print(name)
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
    ggplot2::scale_fill_viridis_c(na.value = NA,
                                  labels = labels,
                                  guide = ggplot2::guide_colourbar(
                                    direction = "horizontal",
                                    barheight = ggplot2::unit(2, units = "mm"),
                                    barwidth = ggplot2::unit(30, units = "mm"),
                                    draw.ulim = F,
                                    title.position = "top",
                                    title.hjust = 0.5,
                                    label.hjust = 0.5,
                                    order = 1
                                  )) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "top",
                   plot.title = ggplot2::element_text(hjust = .5, size = 10),
                   plot.subtitle = ggplot2::element_text(hjust = .5, size = 9),
                   legend.title = ggplot2::element_text(hjust = .5, size = 9)
                   ) +
    ggplot2::labs(
      fill = unit_label,
      subtitle = name |>
        stringr::word(start = 3, sep = "_") |>
        stringr::str_replace("([:alpha:]+)([1-9])", "\\1 \\2") |>
        stringr::str_to_sentence(),
      title = eumohp_measure |> stringr::str_to_upper()
        )
}

#' Plotting the clipped EU-MOHP data
#'
#' Plots the clipped EU-MOHP data as grid using ggplot2.
#'
#' @param .eumohp_starsproxy A list of stars proxy objects as derived
#' from the function eumohp_clip().
#' @param ... Additional arguments.
#' @return ...
#' @examples
#' \dontrun{
#' eumohp_clip(
#'    directory_input = "directory/to/EU-MOHPfiles/",
#'    region_name_spatcov = c("italy2"),
#'    hydrologic_order = 1:4,
#'    abbreviation_measure = c("dsd", "lp"),
#'    eumohp_version = "v013.1.1"
#' ) |>
#' eumohp_plot()
#'
#'
#' # If you want to plot faster, you can increase the argument downsample:
#'
#' eumohp_clip(
#'    directory_input = "directory/to/EU-MOHPfiles/",
#'    region_name_spatcov = c("italy2"),
#'    hydrologic_order = 1:4,
#'    abbreviation_measure = c("dsd", "lp"),
#'    eumohp_version = "v013.1.1"
#' ) |>
#' eumohp_plot(downsample = 200)
#' }
#' @export
eumohp_plot <- function(.eumohp_starsproxy, ...) {
  test <- FALSE
  if (test) {
    .eumohp_starsproxy <- .eumohp_starsproxy
  }

  .eumohp_starsproxy |>
    purrr::imap(.plot_single_order, ...) |>
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
}
