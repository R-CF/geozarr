# CoordinateSystemAxis

lon_crd <- function() CoordinatesPacked$new("lon_coordinates", "EAST", "degrees", c(-170, 20), 18L)

test_that("CoordinateSystemAxis wraps a single Coordinates instance", {
  ax <- CoordinateSystemAxis$new("lon", "X", lon_crd())
  expect_identical(ax$name, "lon")
  expect_identical(ax$abbreviation, "X")
  expect_identical(ax$length, 18L)
  expect_s3_class(ax$coordinates, "CoordinatesPacked")
  expect_length(ax$coordinates_list, 1L)
})

test_that("CoordinateSystemAxis with several coordinate sets activates the first", {
  c1 <- CoordinatesIrregular$new("a", "EAST", "m", c(10, 20, 35))
  c2 <- CoordinatesString$new("b", "OTHER", "-", c("p", "q", "r"))
  ax <- CoordinateSystemAxis$new("x", "X", list(a = c1, b = c2))
  expect_identical(ax$coordinates, c1)
  expect_identical(names(ax$coordinates_list), c("a", "b"))
  expect_identical(nrow(ax$brief()), 1L)
})

test_that("CoordinateSystemAxis validates arguments", {
  expect_error(CoordinateSystemAxis$new("x", 1, lon_crd()), "abbreviation must be a character string")
  expect_error(CoordinateSystemAxis$new("x", c("X", "Y"), lon_crd()), "abbreviation must be a character string")
  expect_error(CoordinateSystemAxis$new("x", "X", list()), "one or more Coordinates")
})

test_that("CoordinateSystemAxis abbreviation can be set", {
  ax <- CoordinateSystemAxis$new("lon", "X", lon_crd())
  ax$abbreviation <- c("Y", "Z")
  expect_identical(ax$abbreviation, "Y")
  ax$abbreviation <- 1
  expect_identical(ax$abbreviation, "Y")
})

test_that("CoordinateSystemAxis read-only fields", {
  ax <- CoordinateSystemAxis$new("lon", "X", lon_crd())
  ax$coordinates <- NULL
  ax$length <- 1L
  expect_identical(ax$length, 18L)
})

test_that("CoordinateSystemAxis direction and unit come from the active coordinates", {
  ax <- CoordinateSystemAxis$new("lon", "X", lon_crd())
  expect_identical(ax$coordinates$direction, "EAST")
  expect_identical(ax$coordinates$unit, "degrees")
})

test_that("CoordinateSystemAxis brief()", {
  b <- CoordinateSystemAxis$new("lon", "X", lon_crd())$brief()
  expect_identical(b, data.frame(abbr = "X", name = "lon", direction = "EAST", length = 18L,
                                 values = "[-170 ... 170]", unit = "degrees"))
  b <- CoordinateSystemAxis$new("h", "Z", CoordinatesIrregular$new("h", "UP", "", 2.5))$brief()
  expect_identical(b$values, "[2.5]")
  expect_identical(b$unit, "-")
})

test_that("CoordinateSystemAxis slice() delegates to the active coordinates", {
  ax <- CoordinateSystemAxis$new("lon", "X", lon_crd())
  expect_identical(ax$slice(c(-155, -125)), c(2L, 3L))
  expect_null(ax$slice(c(500, 600)))
})

test_that("CoordinateSystemAxis copy()", {
  ax <- CoordinateSystemAxis$new("lon", "X", lon_crd())
  cp <- ax$copy()
  expect_false(identical(ax$coordinates, cp$coordinates))
  expect_identical(cp$name, "lon")
  expect_identical(cp$abbreviation, "X")
  expect_identical(cp$coordinates$values, ax$coordinates$values)
})

test_that("CoordinateSystemAxis subset()", {
  ax <- CoordinateSystemAxis$new("lon", "X", lon_crd())
  s <- ax$subset(rng = c(2L, 4L))
  expect_identical(s$name, "lon")
  expect_identical(s$coordinates$values, c(-150, -130, -110))
  expect_identical(ax$subset("east", c(2L, 4L))$name, "east")
  expect_identical(ax$subset()$length, 18L)
})
