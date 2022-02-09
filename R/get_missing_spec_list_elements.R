get_missing_spec_list_elements <- function(missing_placeholder_spec) {
  filename_placeholders_values |>
    {\(x) purrr::keep(x, names(x) == missing_placeholder_spec)}() |>
    as.vector()
}
