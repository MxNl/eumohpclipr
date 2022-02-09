read_and_crop_stars <- function(filepaths_subset, countries) {
  filepaths_subset <- filepaths_subset |>
    dplyr::group_by(dplyr::across(dplyr::all_of(filename_placeholders[2:4])))

  clip_layer <- eumohp_covered_countries |>
    dplyr::filter(stringr::str_to_lower(name) %in% countries) |>
    dplyr::arrange(name) |>
    dplyr::group_by(name = stringr::str_to_lower(stringr::str_c(name, collapse = "-"))) |>
    dplyr::summarise()

  mosaic_names <- filepaths_subset |>
    dplyr::summarise(.groups = "drop") |>
    tidyr::unite(mosaic_names) |>
    dplyr::pull(mosaic_names) |>
    {
      \(x) stringr::str_c(clip_layer$name, x, sep = "_")
    }()

  all_regions <- filepaths_subset |>
    dplyr::group_map(~ .x$value |> read_starsproxy_aslist()) |>
    purrr::map(starsproxylist_as_mosaic) |>
    purrr::set_names(mosaic_names) |>
    purrr::map(~ .x |> st_crop(st_transform(clip_layer, st_crs(.x))))
}
