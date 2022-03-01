library(eumohpclipr)

test_that("Type of value", {
  expect_type(
    eumohp_clip(here::here("tests", "fixtures"),
                region_name_spatcov = "france",
                eumohp_version = "v013.1.1"
    ) |>
      eumohp_plot(),
    "list"
  )
})

test_that("Class of value", {
  expect_s3_class(
    eumohp_clip(here::here("tests", "fixtures"),
                region_name_spatcov = "france",
                eumohp_version = "v013.1.1"
    ) |>
      eumohp_plot(),
    c("ggplot", "gg", "patchwork")
  )
})
