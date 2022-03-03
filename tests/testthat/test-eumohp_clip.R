test_that("Invalid combination of args: countries, custom_, region_", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      countries = c("france"),
      custom_sf_polygon = "test_arg",
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ),
    "You provided more than one argument for the spatial coverage. Please provide exactly one of the following three arguments: countries, custom_sf_polygon, region_name_spatcov" # nolint
  )
})

test_that("No arg provided for: countries, custom_, region_", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      eumohp_version = "v013.1.1"
    ),
    # add error message: problem with newlines
  )
})

test_that("Invalid combination of args: countries, custom_sf_polygon", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      countries = c("france"),
      custom_sf_polygon = "test_arg",
      eumohp_version = "v013.1.1"
    )
  )
})

test_that("Invalid combination of args: countries, region_name_spatcov", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      countries = c("france"),
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ),
    "You provided more than one argument for the spatial coverage. Please provide exactly one of the following three arguments: countries, custom_sf_polygon, region_name_spatcov" # nolint
  )
})

test_that("Invalid combination of args: custom_sf_polygon, region_", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      custom_sf_polygon = "test_arg",
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ),
    "You provided more than one argument for the spatial coverage. Please provide exactly one of the following three arguments: countries, custom_sf_polygon, region_name_spatcov" # nolint
  )
})

test_that("Invalid combination of args: buffer, region_name_spatcov", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      buffer = 1,
      eumohp_version = "v013.1.1"
    ),
    "Please don't provide the argument buffer when using the region_name_spatcov argument!" # nolint
  )
})

test_that("Invalid sf object as custom_sf_polygon", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      custom_sf_polygon = readRDS(
        system.file(
          "extdata",
          "test_custom_sf_polygon.Rds",
          package = "eumohpclipr")),
      eumohp_version = "v013.1.1"
    ),
    "Invalid sf object provided to the argument custom_sf_polygon.
Check if your provided sf object has just a single feature. If not, please use the function summarise from the sf package to merge the features." # nolint
  )
})

test_that("Invalid values for arg: countries", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      countries = c("France", "GREECE"),
      eumohp_version = "v013.1.1"
    ),
    "Invalid values provided to the argument countries:
 France, GREECE
Please check if your provided value\\(s\\) is/are one of:
austria, belgium, bulgaria, croatia, cyprus, czech rep\\., denmark, estonia, finland, france, germany, greece, hungary, iceland, ireland, italy, latvia, liechtenstein, lithuania, luxembourg, malta, netherlands, norway, poland, portugal, romania, slovakia, slovenia, spain, sweden, switzerland, turkey, albania, bosnia and herz\\., kosovo, montenegro, macedonia, serbia, united kingdom" # nolint
  )
})

test_that("Invalid values for arg: region_name_spatcov", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = c("France", "GREECE"),
      eumohp_version = "v013.1.1"
    ),
    "Invalid values provided to the argument region_name_spatcov:
 France, GREECE
Please check if your provided value\\(s\\) is/are one of:
europemainland, finland-norway-sweden, france, greece, iceland, italy1, italy2, turkey, unitedkingdom, unitedkingdom-ireland" # nolint
  )
})

test_that("Invalid values for arg: region_name_spatcov", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      hydrologic_order = c(1:3, "fourth", 10),
      eumohp_version = "v013.1.1"
    ),
    "Invalid values provided to the argument hydrologic_order:
 fourth, 10
Please check if your provided value\\(s\\) is/are one of:
1, 2, 3, 4, 5, 6, 7, 8, 9"
  )
})

test_that("Invalid values for arg: eumohp_version", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1s"
    ),
    "Invalid eumohp version provided to the argument eumohp_version:
v013\\.1\\.1s
Please check if your provided value is one of:
v013\\.1\\.0, v013\\.1\\.1"
  )
})

test_that("Invalid values for arg: abbreviation_measure", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      abbreviation_measure = c(1, "lp", "st")
    ),
    "Invalid values provided to the argument abbreviation_measure:
 1, st
Please check if your provided value\\(s\\) is/are one of:
dsd, lp, sd"
  )
})

test_that("Invalid values for arg: spatial_resolution", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      spatial_resolution = "40m"
    ),
    "Invalid values provided to the argument spatial_resolution:
 40m
Please check if your provided value\\(s\\) is/are one of:
30m"
  )
})

test_that("Length of value", {
  expect_length(
    eumohp_clip(
      directory_input = testthat::test_path("fixtures"),
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ),
    6
  )
})

test_that("Class of value", {
  expect_equal(
    eumohp_clip(
      directory_input = testthat::test_path("fixtures"),
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ) |>
      map(class) |>
      unlist() |>
      unique(),
    c("stars_proxy", "stars")
  )
})
