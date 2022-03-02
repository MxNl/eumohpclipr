# nolint start


test_that("Invalid sf object as custom_sf_polygon 2", {

  f <- function(test){
    rlang::abort(paste0(crayon::red("R"), test, "\nasd"))
  }

  expect_error(
    f("hallo"),
    paste0("R", "hallo", "\nasd")
  )
})

p <- function(x) {
  print(x)
  x
}

# nolint end
