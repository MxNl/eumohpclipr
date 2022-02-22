.is_valid_custom_sf_polygon <- function(custom_sf_polygon) {
  if (!is.null(custom_sf_polygon)) {
    c(
      any(class(custom_sf_polygon) == "sf"),
      nrow(custom_sf_polygon) == 1,
      sf::st_is_simple(custom_sf_polygon),
      sf::st_is(
        sf::st_geometry(custom_sf_polygon),
        c("MULTIPOLYGON", "POLYGON")
      )
    )
  }
}
.is_valid_mode_selection <- function(countries,
                                     custom_sf_polygon,
                                     region_name_spatcov) {
  list(countries, custom_sf_polygon, region_name_spatcov) |>
    map(~ !is.null(.x)) |>
    unlist() |>
    sum() |>
    magrittr::equals(1)
}
.is_valid_countries <- function(countries) {
  eea39_countries <- eea39_countries |> str_to_lower()
  countries %in% eea39_countries
}
.is_valid_eumohp_version <- function(eumohp_version) {
  eumohp_version %in% c("v013.1.0", "v013.1.1")
}
.is_valid_placeholders_value <- function(filename_placeholder) {
  filename_placeholders_values <- filename_placeholders_values |>
    str_remove("streamorder") |>
    purrr::set_names(names(filename_placeholders_values))
  filename_placeholder %in%
    filename_placeholders_values[
      names(filename_placeholders_values) ==
        deparse(substitute(filename_placeholder))]
}
.replace_order_extension <- function(spec_list, order_name) {
  spec_list$hydrologic_order <- str_replace(
    spec_list$hydrologic_order,
    "streamorder",
    order_name)
  spec_list
}
.specs_to_pattern <- function(subset_list) {
  subset_list |>
    map(str_c, collapse = "|") |>
    map(~ if (str_detect(.x, "\\|")) {
      str_c("(", .x, ")")
    } else {
      .x
    }) |>
    str_c(collapse = "_") |> {
      \ (x) str_c("_", x, ".tif")
    }()
}
.get_missing_speclist_elem <- function(missing_placeholder_spec) {
  filename_placeholders_values |> {
    \ (x) keep(x, names(x) == missing_placeholder_spec)
  }() |>
    as.vector()
}
.complete_subsetspecs <- function(subset_specs) {
  missing_placeholder_spec <- filename_placeholders |>
    discard(filename_placeholders %in% names(subset_specs))
  missing_placeholder_spec |>
    map(.get_missing_speclist_elem) |>
    purrr::set_names(missing_placeholder_spec) |>
    c(subset_specs) |>
    magrittr::extract(filename_placeholders)
}
.generate_error_message <- function(argument) {
  eea39_countries <- eea39_countries |> str_to_lower()
  argument_name <- deparse(substitute(argument))
  if (argument_name == "countries") {
    wrong_strings <- str_c(
      argument[!.is_valid_countries(argument)],
      collapse = ", ")
    correct_strings <- str_c(eea39_countries, collapse = ", ")
  } else if (argument_name %in% filename_placeholders) {
    correct_strings <- filename_placeholders_values |> {
      \ (x) keep(x, names(x) == argument_name)
    }() |>
      as.vector()
    wrong_strings <- correct_strings |> {
      \ (x) discard(argument, magrittr::is_in(argument, x))
    }() |>
      str_c(collapse = ", ")
    correct_strings <- correct_strings |>
      str_c(collapse = ", ")
  }
  abort(paste0(
    "Invalid values provided to the argument ",
    crayon::red(argument_name),
    ":\n ",
    crayon::red(wrong_strings),
    "\nPlease check if your provided value(s) is/are one of:\n",
    crayon::green(correct_strings)
  ))
}
.subset_filepaths <- function(filepaths, subset_list) {
  filepaths |>
    dplyr::as_tibble() |>
    mutate(filename = basename(.data$value)) |>
    filter(str_detect(.data$filename, .specs_to_pattern(subset_list))) |>
    mutate(filename_specs = str_remove(.data$filename, "mohp_europe_")) |>
    mutate(filename_specs = str_remove(.data$filename_specs, ".tif")) |>
    tidyr::separate(.data$filename_specs,
                    into = filename_placeholders,
                    sep = "_")
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
    map(stars::read_stars, proxy = TRUE)
}
.read_and_clip_stars <- function(filepaths_subset, clip_layer) {
  filepaths_subset <- filepaths_subset |>
    group_by(dplyr::across(dplyr::all_of(filename_placeholders[2:4])))

  mosaic_names <- filepaths_subset |>
    summarise(.groups = "drop") |>
    tidyr::unite(mosaic_names) |>
    dplyr::pull(mosaic_names) |> {
      \ (x) str_c(clip_layer$name, x, sep = "_")
    }()

  all_regions <- filepaths_subset |>
    dplyr::group_map(~ .x$value |> .read_starsproxy_aslist()) |>
    map(.starsproxylist_as_mosaic) |>
    purrr::set_names(mosaic_names)

  if (!is.null(clip_layer)) {
    all_regions <- all_regions |>
      map(~ .x |> sf::st_crop(sf::st_transform(clip_layer, sf::st_crs(.x))))
  }
  return(all_regions)
}
.eumohp_covered_countries <- function() {
  system.file("extdata",
              "eumohp_covered_countries.rds",
              package = "eumohpclipr",
              mustWork = TRUE) |>
    readRDS()
}
.test_custom_sf_polygon <- function() {
  system.file("extdata",
              "test_custom_sf_polygon.rds",
              package = "eumohpclipr",
              mustWork = TRUE) |>
    readRDS()
}



#' Clip EUMOHP raster data to an arbitrary polygon
#'
#' @param directory_input A string describing a directory
#' where all the EUMOHP .tif files are located.
#' The provided directory will be searched recursively for files starting
#' with 'eumohp_' file name prefix and a .tif ending.
#' @param countries A character vector of arbitrary length.
#' Each element of this vector should be a name of a country
#' that belongs to the EEA39 countries.
#' To see a list of these countries call 'eea39_countries' in the R console.
#' If you provide this argument, you can specify
#' the countries that should be clipped from
#' the EUMOHP data set. The rnaturalearth boundaries
#' will be used as administrative
#' boundaries for clipping.
#' You can only provide on the following three arguments: countries,
#' custom_sf_polygon, region_name_spatcov.
#' @param custom_sf_polygon A simple feature collection with a single feature of
#' geometry type 'POLYGON'.
#' If you have more than one feature,
#' use summarise() to union the features.
#' If you provide this argument,
#' the EUHMOHP data set will be clipped using these polygons.
#' You can only provide on the following three arguments: countries,
#' custom_sf_polygon, region_name_spatcov.
#' @param region_name_spatcov A character vector of arbitrary length.
#' Each element of this vector should be a
#' value of the placeholder region_name_spatcov
#' according to the file naming scheme of the EUMOHP data set.
#' If you provide this argument, you can specify the .tif files
#' of the EUMOHP data set that should be clipped
#' included in the clipped output of this function.
#' You can only provide on the following three arguments: countries,
#' custom_sf_polygon, region_name_spatcov.
#' @param hydrologic_order A integer vector of arbitrary length.
#' Via this argument you can specify the hydrologic orders
#' that you want to obtain in the clipped result of this function.
#' The default is all hydrologic orders (1 - 9).
#' @param abbreviation_measure A character vector of arbitrary length,
#' but maximum length 3.
#' Via this argument you can specify the measures
#' that you want to obtain in the clipped result of this function.
#' The default is all measures (dsd', 'lp' and 'sd').
#' @param spatial_resolution A character vector of length 1.
#' Via this argument you can specify the spatial resolution
#' that you want to obtain in the clipped result of this function.
#' The default is 30m. Currently, there is only a resolution of 30m available.
#' @param eumohp_version A character vector of length 1.
#' Via this argument you specify the EUMOHP version
#' that you are using / have downloaded.
#' The default is v013.1.0.
#' @param buffer A numeric vector of length 1 (optionally).
#' Via this argument you can specify a buffer in meters
#' that should be applied to the the provided polygons / countries.
#' If you provided the argument region_name_spatcov,
#' this argument is irrelevant.
#' The default is NULL.
#' @return A list of stars_proxy objects with the user
#' specified spatial extent / shape.
#' @examples
#' \dontrun{
#' Specifying the spatial extent of the clipped result
#' via the argument: countries
#' eumohp_clip(
#'    directory_input = "directory/to/EUMOHPfiles/",
#'    countries = "germany",
#'    buffer = 1E4,
#'    hydrologic_order = 1:4,
#'    abbreviation_measure = c("dsd", "lp"),
#'    eumohp_version = "v013.1.1"
#' )
#' Specifying the spatial extent of the clipped result
#' via the argument: custom_sf_polygon
#' eumohp_clip(
#'    directory_input = "directory/to/EUMOHPfiles/",
#'    custom_sf_polygon = .test_custom_sf_polygon() |> summarise(),
#'    buffer = 1E4,
#'    hydrologic_order = 1:4,
#'    abbreviation_measure = c("dsd", "lp"),
#'    eumohp_version = "v013.1.1"
#' )
#' Specifying the spatial extent of the clipped result
#' via the argument: region_name_spatcov
#' eumohp_clip(
#'    directory_input = "directory/to/EUMOHPfiles/",
#'    region_name_spatcov = c("france", "turkey", "italy2"),
#'    hydrologic_order = 1:4,
#'    abbreviation_measure = c("dsd", "lp"),
#'    eumohp_version = "v013.1.1"
#' )
#' }
#' @export
eumohp_clip <- function(directory_input,
                        countries = NULL,
                        custom_sf_polygon = NULL,
                        region_name_spatcov = NULL,
                        hydrologic_order = 1:9,
                        abbreviation_measure = c("dsd", "lp", "sd"),
                        spatial_resolution = "30m",
                        eumohp_version = "v013.1.0",
                        buffer = NULL) {
  filepaths <- directory_input |>
    list.files(full.names = TRUE, recursive = TRUE) |> {
      \ (x) x |> keep(str_detect(x, "mohp_europe_*.*tif"))
    }()
  eea39_countries <- eea39_countries |> str_to_lower()
  filename_placeholders_values <- filename_placeholders_values |>
    str_remove("streamorder") |>
    purrr::set_names(names(filename_placeholders_values))
  order_name <- ifelse(eumohp_version == eumohp_versions[1],
                       "streamorder",
                       "hydrologicorder"
  )

  if (!.is_valid_mode_selection(
    countries,
    custom_sf_polygon,
    region_name_spatcov
  )) {
    abort(
      paste0(
        "You provided more than one argument for the spatial coverage. ",
        "Please provide exactly one of the following three arguments: ",
        crayon::green(str_c(c(
          "countries",
          "custom_sf_polygon",
          "region_name_spatcov"
        ),
        collapse = ", "
        ))
      )
    )
  }
  if (!is.null(countries) & !(.is_valid_countries(countries) |> all())) {
    .generate_error_message(countries)
  }
  if (!is.null(custom_sf_polygon) &
      !(.is_valid_custom_sf_polygon(custom_sf_polygon) |>
        all())) {
    abort(paste0(
      "Invalid sf object provided to the argument ",
      crayon::red("custom_sf_polygon"),
      "."
    ))
  }
  if (!is.null(region_name_spatcov) &
      !(.is_valid_placeholders_value(region_name_spatcov) |>
        all())) {
    .generate_error_message(region_name_spatcov)
  }
  if (!is.null(hydrologic_order) &
      !(.is_valid_placeholders_value(hydrologic_order) |>
        all())) {
    .generate_error_message(hydrologic_order)
  }
  if (!is.null(abbreviation_measure) &
      !(.is_valid_placeholders_value(abbreviation_measure) |>
        all())) {
    .generate_error_message(abbreviation_measure)
  }
  if (!is.null(spatial_resolution) &
      !(.is_valid_placeholders_value(spatial_resolution) |>
        all())) {
    .generate_error_message(spatial_resolution)
  }
  if (!.is_valid_eumohp_version(eumohp_version)) {
    abort(paste0(
      "Invalid eumohp version provided to the argument ",
      crayon::red("eumohp_version"),
      ":\n",
      crayon::red(eumohp_version),
      "\nPlease check if your provided value is one of:\n",
      crayon::green(str_c(c("v013.1.0", "v013.1.1"),
                          collapse = ", "
      ))
    ))
  }

  if (!is.null(countries) & (.is_valid_countries(countries) |> all())) {
    clip_layer <- .eumohp_covered_countries() |>
      filter(str_to_lower(.data$name) %in% countries) |>
      arrange(.data$name) |>
      group_by(name = str_to_lower(str_c(.data$name, collapse = "-"))) |>
      summarise()
  } else if (!is.null(custom_sf_polygon) &
             (.is_valid_custom_sf_polygon(custom_sf_polygon) |>
              all())) {
    clip_layer <-
      custom_sf_polygon |>
      sf::st_geometry() |>
      sf::st_as_sf() |>
      sf::st_cast("MULTIPOLYGON") |>
      dplyr::rename(geometry = .data$x) |>
      mutate(name = "custompolygon", .before = 1)
  } else {
    clip_layer <- NULL
  }

  if (is.numeric(buffer)) {
    clip_layer <- clip_layer |>
      sf::st_buffer(dist = buffer)
  }

  subset_specs <-
    as.list(environment()) |>
    keep(.p = \ (x) {
      names(x) %in% filename_placeholders |> any()
    }) |>
    .complete_subsetspecs() |>
    .replace_order_extension(order_name)

  filepaths_subset <- filepaths |> .subset_filepaths(subset_specs)
  .read_and_clip_stars(filepaths_subset, clip_layer)
}
