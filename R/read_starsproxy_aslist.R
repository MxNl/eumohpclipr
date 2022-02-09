read_starsproxy_aslist <- function(filepaths) {
  filepaths |>
    purrr::map(stars::read_stars, proxy = TRUE)
}
