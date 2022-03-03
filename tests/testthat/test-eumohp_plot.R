test_that("Type of value", {
  expect_type(
    eumohp_clip(
      directory_input = testthat::test_path("fixtures"),
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ) |>
      eumohp_plot(downsample = 1),
    "list"
  )
})

test_that("Class of value", {
  expect_s3_class(
    eumohp_clip(
      directory_input = testthat::test_path("fixtures"),
      region_name_spatcov = "france",
      eumohp_version = "v013.1.1"
    ) |>
      eumohp_plot(downsample = 1),
    c("ggplot", "gg", "patchwork")
  )
})
