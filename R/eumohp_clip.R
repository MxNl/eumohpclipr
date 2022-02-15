.is_valid_custom_sf_polygon <- function(custom_sf_polygon) {
  if (!is.null(custom_sf_polygon)) {c(any(class(custom_sf_polygon) == "sf"),
    nrow(custom_sf_polygon) == 1,
    sf::st_is_simple(custom_sf_polygon),
    sf::st_is(sf::st_geometry(custom_sf_polygon), c("MULTIPOLYGON", "POLYGON"))
    )}
  }
.is_valid_mode_selection <- function(countries, custom_sf_polygon, region_name_spatcov) {
  list(countries, custom_sf_polygon, region_name_spatcov) |>
    purrr::map(~ !is.null(.x)) |>
    unlist() |>
    sum() |>
    magrittr::equals(1)
  }
.is_valid_countries <- function(countries) {
  eea39_countries <- eea39_countries |> stringr::str_to_lower()
  countries %in% eea39_countries
  }
.is_valid_eumohp_version <- function(eumohp_version) eumohp_version %in% c("v013.1.0", "v013.1.1")
.is_valid_placeholders_value <- function(filename_placeholder) {
  filename_placeholders_values <- filename_placeholders_values |>
    stringr::str_remove("streamorder") |>
    purrr::set_names(names(filename_placeholders_values))
  filename_placeholder %in% filename_placeholders_values[names(filename_placeholders_values) == deparse(substitute(filename_placeholder))]}
.replace_order_extension <- function(spec_list, order_name) {
    spec_list$hydrologic_order <- stringr::str_replace(spec_list$hydrologic_order, "streamorder", order_name)
    return(spec_list)
}
.specs_to_pattern <- function(subset_list) {
  subset_list |>
    purrr::map(stringr::str_c, collapse = "|") |>
    purrr::map(~ if (stringr::str_detect(.x, "\\|")) stringr::str_c("(", .x, ")") else .x) |>
    stringr::str_c(collapse = "_") |>
    {\(x) stringr::str_c("_", x, ".tif")}()
}
.get_missing_spec_list_elements <- function(missing_placeholder_spec) {
  filename_placeholders_values |>
    {\(x) purrr::keep(x, names(x) == missing_placeholder_spec)}() |>
    as.vector()
}
.complete_subsetspecs <- function(subset_specs) {
  missing_placeholder_spec <- filename_placeholders |>
    purrr::discard(filename_placeholders %in% names(subset_specs))
  missing_placeholder_spec |>
    purrr::map(.get_missing_spec_list_elements) |>
    purrr::set_names(missing_placeholder_spec) |>
    c(subset_specs) |>
    magrittr::extract(filename_placeholders)
}
.generate_error_message <- function(argument) {
  eea39_countries <- eea39_countries |> stringr::str_to_lower()
  argument_name <- deparse(substitute(argument))
    if (argument_name == "countries") {
      wrong_strings <- stringr::str_c(argument[!.is_valid_countries(argument)], collapse = ", ")
      correct_strings <- stringr::str_c(eea39_countries, collapse = ", ")
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
.subset_filepaths <- function(filepaths, subset_list) {
  filepaths |>
    dplyr::as_tibble() |>
    dplyr::mutate(filename = basename(value)) |>
    dplyr::filter(stringr::str_detect(filename, .specs_to_pattern(subset_list))) |>
    dplyr::mutate(filename_specs = stringr::str_remove(filename, "mohp_europe_")) |>
    dplyr::mutate(filename_specs = stringr::str_remove(filename_specs, ".tif")) |>
    tidyr::separate(filename_specs, into = filename_placeholders, sep = "_")
}
.starsproxylist_as_mosaic <- function(starsproxylist) {
  mosaic <- stars::st_mosaic(starsproxylist[[1]])

  if (length(starsproxylist) > 1) {
    for (i in 2:length(starsproxylist)) {
      mosaic <-
        stars::st_mosaic(mosaic, starsproxylist[[i]])
    }
  }
  mosaic
}
.read_starsproxy_aslist <- function(filepaths) {
  filepaths |>
    purrr::map(stars::read_stars, proxy = TRUE)
}
.read_and_clip_stars <- function(filepaths_subset, clip_layer) {
  filepaths_subset <- filepaths_subset |>
    dplyr::group_by(dplyr::across(dplyr::all_of(filename_placeholders[2:4])))
  # dplyr::group_by(region_name_spatcov)

  mosaic_names <- filepaths_subset |>
    dplyr::summarise(.groups = "drop") |>
    tidyr::unite(mosaic_names) |>
    dplyr::pull(mosaic_names) |>
    {
      \(x) stringr::str_c(clip_layer$name, x, sep = "_")
    }()

  all_regions <- filepaths_subset |>
    dplyr::group_map(~ .x$value |> .read_starsproxy_aslist()) |>
    purrr::map(.starsproxylist_as_mosaic) |>
    purrr::set_names(mosaic_names)

  if (!is.null(clip_layer)) {
    all_regions <- all_regions |>
      purrr::map(~ .x |> sf::st_crop(sf::st_transform(clip_layer, sf::st_crs(.x))))
  }
  return(all_regions)
}


#' @export
#' @importFrom dplyr .data
eumohp_clip <- function(directory_input,
                        countries = NULL,
                        custom_sf_polygon = NULL,
                        region_name_spatcov = NULL,
                        hydrologic_order = 1:9,
                        abbreviation_measure = c("dsd", "lp", "sd"),
                        spatial_resolution = "30m",
                        eumohp_version = "v013.1.0",
                        buffer = NULL) {
  test <- FALSE
  if (test) {
    directory_input <- here::here(
      "..", "macro_mohp_feature_test", "macro_mohp_feature",
      "output_data", "lateral_position"
    )
    # countries <- NULL
    countries <- c("Germany")
    custom_sf_polygon <- NULL
    # custom_sf_polygon <- test_custom_sf_polygon |> dplyr::summarise()
    region_name_spatcov <- NULL
    # region_name_spatcov <- c("france", "turkey")
    hydrologic_order <- 1:9
    abbreviation_measure <- c("dsd", "lp", "sd")
    spatial_resolution <- "30m"
    eumohp_version <- "v013.1.1"
    buffer = 1E4
  }

  filepaths <- directory_input |>
    list.files(full.names = TRUE, recursive = TRUE) |>
    {\(x) x |> purrr::keep(stringr::str_detect(x, "mohp_europe_*.*tif"))}()
  eea39_countries <- eea39_countries |> stringr::str_to_lower()
  filename_placeholders_values <- filename_placeholders_values |>
    stringr::str_remove("streamorder") |>
    purrr::set_names(names(filename_placeholders_values))
  order_name <- ifelse(eumohp_version == eumohp_versions[1], "streamorder", "hydrologicorder")

  if(!.is_valid_mode_selection(countries, custom_sf_polygon, region_name_spatcov)) {
    rlang::abort(paste0("You provided more than one argument for the spatial coverage. Please provide exactly one of the following three arguments: ",
    crayon::green(stringr::str_c(c("countries", "custom_sf_polygon", "region_name_spatcov"), collapse = ", "))))
  }
  if (!is.null(countries) & !(.is_valid_countries(countries) |> all())) {
    .generate_error_message(countries)
  }
  if (!is.null(custom_sf_polygon) & !(.is_valid_custom_sf_polygon(custom_sf_polygon) |> all())) {
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
  if (!.is_valid_eumohp_version(eumohp_version)) {
    rlang::abort(paste0("Invalid eumohp version provided to the argument ",
                        crayon::red("eumohp_version"),
                        ":\n",
                        crayon::red(eumohp_version),
                        "\nPlease check if your provided value is one of:\n",
                        crayon::green(stringr::str_c(c("v013.1.0", "v013.1.1"), collapse = ", "))))
  }

  if (!is.null(countries) & (.is_valid_countries(countries) |> all())) {
    clip_layer <- eumohp_covered_countries |>
      dplyr::filter(stringr::str_to_lower(name) %in% countries) |>
      dplyr::arrange(name) |>
      dplyr::group_by(name = stringr::str_to_lower(stringr::str_c(name, collapse = "-"))) |>
      dplyr::summarise()
  } else  if (!is.null(custom_sf_polygon) & (.is_valid_custom_sf_polygon(custom_sf_polygon) |> all())) {
    clip_layer <-
      custom_sf_polygon |>
      sf::st_geometry() |>
      sf::st_as_sf() |>
      sf::st_cast("MULTIPOLYGON") |>
      dplyr::rename(geometry = x) |>
      dplyr::mutate(name = "custompolygon", .before = 1)
  } else {
    clip_layer <- NULL
  }

  if (is.numeric(buffer)) {
    clip_layer <- clip_layer |>
      sf::st_buffer(dist = buffer)
  }

  subset_specs <-
    as.list(environment()) |>
    purrr::keep(.p = \(x) {names(x) %in% filename_placeholders |> any()}) |>
    .complete_subsetspecs() |>
    .replace_order_extension(order_name)

  filepaths_subset <- filepaths |> .subset_filepaths(subset_specs)
  .read_and_clip_stars(filepaths_subset, clip_layer)
  # continue here with setting extent and merging stars with c(along = "hydrologic_order)
}
