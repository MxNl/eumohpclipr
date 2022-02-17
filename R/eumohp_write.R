#' @export
eumohp_write <- function(eumohp_starsproxy, directory_output, parallel = FALSE) {
  test <- FALSE
  if (test) {
    eumohp_starsproxy <- eumohp_starsproxy
    directory_output <- here::here(
      "..", "output_test"
    )
  }

  if (parallel) {
    eumohp_starsproxy |>
      furrr::future_imap(
        ~ stars::write_stars(
          .x,
          stringr::str_glue("{directory_output}/mohp_custom_{.y}.tif")
        )
      )
  } else {
    eumohp_starsproxy |>
      purrr::imap(
        ~ stars::write_stars(
          .x,
          stringr::str_glue("{directory_output}/mohp_custom_{.y}.tif")
        )
      )
  }
}
