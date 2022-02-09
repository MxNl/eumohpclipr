.is_valid_custom_sf_polygon <- function() {
  c(any(class(custom_sf_polygon) == "sf"),
    nrow(custom_sf_polygon) == 1,
    sf::st_is_simple(custom_sf_polygon),
    sf::st_is(sf::st_geometry(custom_sf_polygon), c("MULTIPOLYGON", "POLYGON"))
    )
  }
.is_valid_mode_selection <- function() {
  test <- list(countries, custom_sf_polygon, region_name_spatcov) |>
    purrr::map(~ !is.null(.x)) |>
    unlist() |>
    sum()
  return(test > 1)
  }
.is_valid_countries <- function() countries %in% eea39_countries
.is_valid_eumohp_version <- function() eumohp_version %in% c("v013.1.0", "v013.1.1")
.is_valid_placeholders_value <- function(filename_placeholder) filename_placeholder %in% filename_placeholders_values[names(filename_placeholders_values) == deparse(substitute(filename_placeholder))]
.with_order_extension <- function(spec_list) {
    spec_list$hydrologic_order <- paste0(order_name, spec_list$hydrologic_order)
    return(spec_list)
  }
.generate_error_message <- function(argument) {

  argument_name <- deparse(substitute(argument))
    if (argument_name == "countries") {
      wrong_strings <- stringr::str_c(countries[!.is_valid_countries()], collapse = ", ")
      correct_strings <- stringr::str_c(eea39_countries, collapse = ", ")
      # compare_with <- countries
    } else if (argument_name %in% filename_placeholders) {
      correct_strings <- filename_placeholders_values |>
        {\(x) purrr::keep(x, names(x) == argument_name)}() |>
        as.vector()
      wrong_strings <- correct_strings |>
        {\(x) purrr::discard(argument, magrittr::is_in(argument, x))}() |>
        stringr::str_c(collapse = ", ")
      correct_strings <- correct_strings |>
        stringr::str_c(collapse = ", ")
    }
    rlang::abort(paste0(
      "Invalid values provided to the argument ", crayon::red(argument_name), ":\n ",
      crayon::red(wrong_strings),
      "\nPlease check if your provided value(s) is/are one of:\n",
      crayon::green(correct_strings)
    ))
  }

#' @export
eumohpclip <- function(filepaths,
                       countries = NULL,
                       custom_sf_polygon = NULL,
                       region_name_spatcov = NULL,
                       hydrologic_order = 1:9,
                       abbreviation_measure = c("dsd", "lp", "sd"),
                       spatial_resolution = "30m",
                       eumohp_version = "v013.1.0") {
  test <- FALSE
  if (test) {
    filepaths <- here::here(
      "..", "macro_mohp_feature",
      "output_data", "lateral_position"
    ) |> list.files(full.names = TRUE)
    countries <- c("italy", "greece")
    custom_sf_polygon <- test_custom_sf_polygon |> dplyr::summarise()
    region_name_spatcov <- c("france", "turkey")
    hydrologic_order <- NULL
    abbreviation_measure <- c("dsd", "lp", "sd")
    spatial_resolution <- "30m"
    eumohp_version <- "v013.1.0"
  }

  filename_placeholders_values <- filename_placeholders_values |>
    stringr::str_remove("streamorder") |>
    purrr::set_names(names(filename_placeholders_values))
  eea39_countries <- eea39_countries |> stringr::str_to_lower()
  order_name <- ifelse(eumohp_version == eumohp_versions[1], "streamorder", "hydrologicorder")

  if(.is_valid_mode_selection()) {
    rlang::abort(paste0("You provided more than one argument for the spatial coverage. Please provide exactly one of the following three arguments: ",
    crayon::green(stringr::str_c(c("countries", "custom_sf_polygon", "region_name_spatcov"), collapse = ", "))))
  }
  if (!is.null(countries) & !(.is_valid_countries() |> all())) {
    .generate_error_message(countries)
  }
  if (!is.null(custom_sf_polygon) & !(.is_valid_custom_sf_polygon() |> all())) {
    rlang::abort(paste0("Invalid sf object provided to the argument ", crayon::red("custom_sf_polygon"), "."))
  }
  if (!is.null(region_name_spatcov) & !(.is_valid_placeholders_value(region_name_spatcov) |> all())) {
    .generate_error_message(region_name_spatcov)
  }
  if (!is.null(hydrologic_order) & !(.is_valid_placeholders_value(hydrologic_order) |> all())) {
    .generate_error_message(hydrologic_order)
  }
  if (!is.null(abbreviation_measure) & !(.is_valid_placeholders_value(abbreviation_measure) |> all())) {
    .generate_error_message(abbreviation_measure)
  }
  if (!is.null(spatial_resolution) & !(.is_valid_placeholders_value(spatial_resolution) |> all())) {
    .generate_error_message(spatial_resolution)
  }
  if (!.is_valid_eumohp_version()) {
    rlang::abort(paste0("Invalid eumohp version provided to the argument ",
                        crayon::red("eumohp_version"),
                        ":\n",
                        crayon::red(eumohp_version),
                        "\nPlease check if your provided value is one of:\n",
                        crayon::green(stringr::str_c(c("v013.1.0", "v013.1.1"), collapse = ", "))))
  }

  specs_arguments <- c("region_name_spatcov",
                       "hydrologic_order",
                       "abbreviation_measure",
                       "spatial_resolution")
  subset_specs <- specs_arguments |>
    purrr::map(as.symbol) |>
    purrr::map(eval) |>
    purrr::set_names(specs_arguments) |>
    purrr::compact() |>
    complete_subsetspecs() |>
    .with_order_extension()

  filepaths_subset <- filepaths |> subset_filepaths(subset_specs_mod)
  eumohp_raster_clip <- read_and_crop_stars(filepaths_subset, countries)
  # ...continue here
}
