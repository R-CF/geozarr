# CoordinateSystem

make_axes <- function() list(
  x = CoordinateSystemAxis$new("x", "X", CoordinatesPacked$new("x_c", "EAST", "m", c(15, 10), 4L)),
  y = CoordinateSystemAxis$new("y", "Y", CoordinatesPacked$new("y_c", "NORTH", "m", c(75, -10), 3L))
)

test_that("CoordinateSystem stores its axes", {
  cs <- CoordinateSystem$new("cs", make_axes(), attributes = list(a = 1))
  expect_identical(cs$name, "cs")
  expect_identical(cs$getDimension(), 2L)
  expect_identical(names(cs$axes), c("x", "y"))
  expect_identical(cs$getAxis(2L)$name, "y")
  expect_identical(cs$getAxis("x")$name, "x")
  expect_identical(cs$attributes, list(a = 1))
})

test_that("CoordinateSystem validates its axes", {
  expect_error(CoordinateSystem$new("cs", list()), "CoordinateSystemAxis")
  expect_error(CoordinateSystem$new("cs", make_axes()$x), "CoordinateSystemAxis")
  expect_error(CoordinateSystem$new("cs", list(1, 2)), "CoordinateSystemAxis")
})

test_that("CoordinateSystem crs", {
  cs <- CoordinateSystem$new("cs", make_axes())
  expect_identical(cs$crs, "")
  expect_identical(CoordinateSystem$new("cs", make_axes(), crs = NULL)$crs, "")
  cs$crs <- c("EPSG:4326", "x")
  expect_identical(cs$crs, "EPSG:4326")
  cs$crs <- 4326
  expect_identical(cs$crs, "EPSG:4326")
})

test_that("CoordinateSystem axes are read-only", {
  cs <- CoordinateSystem$new("cs", make_axes())
  cs$axes <- list()
  expect_identical(cs$getDimension(), 2L)
})
