subset_filepaths <- function(filepaths, subset_list) {
  filepaths |>
    dplyr::as_tibble() |>
    dplyr::mutate(filename = basename(value)) |>
    dplyr::filter(stringr::str_detect(filename, specs_to_pattern(subset_list))) |>
    dplyr::mutate(filename_specs = stringr::str_remove(filename, "mohp_europe_")) |>
    dplyr::mutate(filename_specs = stringr::str_remove(filename_specs, ".tif")) |>
    tidyr::separate(filename_specs, into = filename_placeholders, sep = "_")
}
