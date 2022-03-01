test_that("Invalid combination of args: countries, custom_, region_", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      countries = c("france"),
      custom_sf_polygon = .test_custom_sf_polygon(),
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
      custom_sf_polygon = .test_custom_sf_polygon(),
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
      custom_sf_polygon = .test_custom_sf_polygon(),
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
      custom_sf_polygon = .test_custom_sf_polygon(),
      eumohp_version = "v013.1.1"
    )
    # add error message: problem with newlines
  )
})

test_that("Invalid values for arg: countries", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      countries = c("France", "GREECE"),
      eumohp_version = "v013.1.1"
    )
    # add error message: problem with newlines
  )
})

test_that("Invalid values for arg: region_name_spatcov", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = c("France", "GREECE"),
      eumohp_version = "v013.1.1"
    )
    # add error message: problem with newlines
  )
})

test_that("Invalid values for arg: region_name_spatcov", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      hydrologic_order = c(1:3, "fourth", 10),
      eumohp_version = "v013.1.1"
    )
    # add error message: problem with newlines
  )
})

test_that("Invalid values for arg: eumohp_version", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1s"
    )
    # add error message: problem with newlines
  )
})

test_that("Invalid values for arg: abbreviation_measure", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      abbreviation_measure = c("dsd", "lp", "st")
    )
    # add error message: problem with newlines
  )
})

test_that("Invalid values for arg: spatial_resolution", {
  expect_error(
    eumohp_clip(
      directory_input = NULL,
      region_name_spatcov = "france",
      spatial_resolution = "40m"
    )
    # add error message: problem with newlines
  )
})

test_that("Length of value", {
  expect_length(
    eumohp_clip(
      directory_input = system.file("tests/fixtures", package = "eumohpclipr"),
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ),
    18
  )
})

test_that("Class of value", {
  expect_equal(
    eumohp_clip(system.file("tests/fixtures", package = "eumohpclipr"),
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ) |>
      map(class) |>
      unlist() |>
      unique(),
    c("stars_proxy", "stars")
  )
})
