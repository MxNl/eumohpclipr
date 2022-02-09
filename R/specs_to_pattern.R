specs_to_pattern <- function(subset_list) {
  subset_list |>
    purrr::map(stringr::str_c, collapse = "|") |>
    purrr::map(~ if (stringr::str_detect(.x, "\\|")) stringr::str_c("(", .x, ")") else .x) |>
    stringr::str_c(collapse = "_") |>
    {\(x) stringr::str_c("_", x, ".tif")}()
}
