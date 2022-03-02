library(eumohpclipr)

test_that("Type of value", {
  expect_type(
    eumohp_clip(system.file(".", package = "eumohpclipr"),
                region_name_spatcov = "france",
                eumohp_version = "v013.1.1"
    ) |>
      eumohp_plot(),
    "list"
  )
})

test_that("Class of value", {
  expect_s3_class(
    eumohp_clip(system.file(".", package = "eumohpclipr"),
                region_name_spatcov = "france",
                eumohp_version = "v013.1.1"
    ) |>
      eumohp_plot(),
    c("ggplot", "gg", "patchwork")
  )
})
