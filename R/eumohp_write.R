#' @export
eumohp_write <- function(eumohp_starsproxy, directory_output) {
  test <- FALSE
  if (test) {
    eumohp_starsproxy <- eumohp_starsproxy
    directory_output <- here::here(
      "..", "output_test"
    )
  }

  eumohp_starsproxy |>
    # purrr::imap(~ stringr::str_glue("{directory_output}/mohp_custom_{.y}"))
    purrr::imap(
      ~ stars::write_stars(
        .x,
        stringr::str_glue("{directory_output}/mohp_custom_{.y}.tif")
      )
    )
}
