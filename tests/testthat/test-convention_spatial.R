# zarr_conv_spatial

test_that("spatial convention identity", {
  s <- zarr_conv_spatial$new()
  expect_identical(s$name, "spatial")
  expect_identical(s$uuid, "689b58e2-cf7b-45e0-9fff-9cfc0883d6b4")
  expect_match(s$schema, "spatial/refs/tags/v0.1/schema.json$")
  expect_identical(s$transform_type, "affine")
  expect_identical(s$registration, "pixel")
})

test_that("spatial as_list() requires dimensions and transform", {
  s <- zarr_conv_spatial$new()
  expect_error(s$as_list(), "spatial:dimensions")
  s$dimensions <- c("y", "x")
  expect_error(s$as_list(), "spatial:transform")
})

test_that("spatial set_coordinates() with pixel registration", {
  s <- zarr_conv_spatial$new()
  s$dimensions <- c("lat", "lon")
  expect_invisible(s$set_coordinates(x = c(0, 10), y = c(100, -10), shape = c(5L, 4L)))
  expect_identical(s$transform, c(10, 0, 0, 0, -10, 100))
  expect_identical(s$bbox, c(0, 60, 50, 100))
  expect_identical(s$as_list(), list(
    `spatial:dimensions`     = c("lat", "lon"),
    `spatial:registration`   = "pixel",
    `spatial:transform_type` = "affine",
    `spatial:transform`      = c(10, 0, 0, 0, -10, 100),
    `spatial:bbox`           = c(0, 60, 50, 100)
  ))
})

test_that("spatial set_coordinates() with node registration", {
  s <- zarr_conv_spatial$new()
  s$set_coordinates(x = c(0, 10), y = c(100, -10), shape = c(5L, 4L), registration = "node")
  expect_identical(s$registration, "node")
  expect_identical(s$bbox, c(0, 70, 40, 100))
})

test_that("spatial set_coordinates() validates arguments", {
  s <- zarr_conv_spatial$new()
  expect_error(s$set_coordinates(x = 0, y = c(100, -10), shape = c(5L, 4L)), "coordinates and resolution")
  expect_error(s$set_coordinates(x = c(0, 10), y = "a", shape = c(5L, 4L)), "coordinates and resolution")
  expect_error(s$set_coordinates(x = c(0, 10), y = c(100, -10), shape = c(5, 4)), "integer vector of length 2")
  expect_error(s$set_coordinates(x = c(0, 10), y = c(100, -10), shape = c(5L, 4L), registration = "corner"),
               '"node" or "pixel"')
})

test_that("spatial optional shape is included", {
  s <- zarr_conv_spatial$new()
  s$dimensions <- c("y", "x")
  s$transform <- c(1, 0, 0, 0, -1, 10)
  s$shape <- c(10L, 20L)
  expect_identical(s$as_list()[["spatial:shape"]], c(10L, 20L))
})

test_that("spatial setters validate", {
  s <- zarr_conv_spatial$new()
  expect_error(s$dimensions <- c("a", "b", "c"), "1 or 2 dimension names")
  expect_error(s$dimensions <- 1:2, "1 or 2 dimension names")
  expect_error(s$bbox <- 1:3, "4 values")
  expect_error(s$transform <- 1:5, "6 values")
  expect_error(s$transform_type <- "affine", "Can't change")
  s$dimensions <- c("y", "x")
  expect_error(s$shape <- 10L, "same length")
  expect_error(s$shape <- c(10, 20), "same length")
  expect_error(s$registration <- "corner", "Bad value")
  s$registration <- "node"
  expect_identical(s$registration, "node")
  s$bbox <- c(0, 0, 1, 1)
  expect_identical(s$bbox, c(0, 0, 1, 1))
})
