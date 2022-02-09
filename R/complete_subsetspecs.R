complete_subsetspecs <- function(subset_specs) {
  missing_placeholder_spec <- filename_placeholders |>
    purrr::discard(filename_placeholders %in% names(subset_specs))
  missing_placeholder_spec |>
    purrr::map(get_missing_spec_list_elements) |>
    purrr::set_names(missing_placeholder_spec) |>
    c(subset_specs) |>
    magrittr::extract(filename_placeholders)
}
