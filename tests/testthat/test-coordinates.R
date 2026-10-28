# Coordinates and descendant classes, plus .make_coordinate_values()

gz <- asNamespace("geozarr")

# CoordinatesIrregular ---------------------------------------------------------

test_that("CoordinatesIrregular constructor stores its fields", {
  crd <- CoordinatesIrregular$new("lon", "EAST", "degrees_east", c(10, 20, 35), attributes = list(a = 1))
  expect_identical(crd$name, "lon")
  expect_identical(crd$direction, "EAST")
  expect_identical(crd$unit, "degrees_east")
  expect_identical(crd$values, c(10, 20, 35))
  expect_identical(crd$raw, c(10, 20, 35))
  expect_identical(crd$range, c(10, 35))
  expect_identical(crd$length, 3L)
  expect_null(crd$bounds)
  expect_null(crd$bounds_raw)
  expect_identical(crd$attributes, list(a = 1))
})

test_that("CoordinatesIrregular constructor validates arguments", {
  expect_error(CoordinatesIrregular$new("c", "SIDEWAYS", "m", 1:3), "AxisDirection")
  expect_error(CoordinatesIrregular$new("c", c("EAST", "WEST"), "m", 1:3), "AxisDirection")
  expect_error(CoordinatesIrregular$new("c", "EAST", 1, 1:3), "unit must be a character string")
  expect_error(CoordinatesIrregular$new("c", "EAST", "m", matrix(1:4, 2)), "must be a vector")
  expect_error(CoordinatesIrregular$new("c", "EAST", "m", c(1, 3, 2)), "monotonic")
})

test_that("CoordinatesIrregular accept decreasing values", {
  crd <- CoordinatesIrregular$new("lat", "NORTH", "degrees", c(60, 40, 35))
  expect_identical(crd$range, c(35, 60))
})

test_that("Coordinates normalise direction case", {
  expect_identical(CoordinatesIrregular$new("c", "east", "m", c(1, 5))$direction, "EAST")
})

test_that("Coordinates active fields can be set", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(1, 5))
  crd$name <- c("new", "ignored")
  expect_identical(crd$name, "new")
  crd$direction <- "WEST"
  expect_identical(crd$direction, "WEST")
  crd$direction <- "NOT_A_DIRECTION"
  expect_identical(crd$direction, "WEST")
  crd$unit <- "km"
  expect_identical(crd$unit, "km")
  crd$unit <- ""
  expect_identical(crd$unit, "km")
})

test_that("Coordinates packed boundaries are expanded on retrieval", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35), bounds = c(-1, 2))
  expect_identical(crd$bounds_raw, c(-1, 2))
  expect_identical(crd$bounds, rbind(c(9, 19, 34), c(12, 22, 37)))
})

test_that("Coordinates regular boundary matrix is packed", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35),
                         bounds = rbind(c(9.5, 19.5, 34.5), c(10.5, 20.5, 35.5)))
  expect_equal(crd$bounds_raw, c(-0.5, 0.5))
})

test_that("Coordinates irregular boundary matrix is kept", {
  b <- rbind(c(5, 15, 30), c(15, 30, 40))
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35), bounds = b)
  expect_identical(crd$bounds_raw, b)
  expect_identical(crd$bounds, b)
})

test_that("Coordinates boundary values are validated", {
  expect_error(CoordinatesIrregular$new("c", "EAST", "m", c(10, 20), bounds = rbind(c(11, 21), c(12, 22))),
               "envelope")
  expect_error(CoordinatesIrregular$new("c", "EAST", "m", c(10, 20), bounds = matrix(1:6, 3)),
               "numeric matrix with 2 rows")
  expect_error(CoordinatesIrregular$new("c", "EAST", "m", c(10, 20), bounds = c(-1, 0, 1)),
               "numeric matrix with 2 rows")
})

test_that("Coordinates boundary values can be assigned", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35))
  crd$bounds <- rbind(c(5, 15, 30), c(15, 30, 40))
  expect_identical(crd$bounds, rbind(c(5, 15, 30), c(15, 30, 40)))
})

test_that("Coordinates brief()", {
  b <- CoordinatesIrregular$new("c", "EAST", "m", c(1.123456, 20, 35))$brief()
  expect_identical(b, data.frame(name = "c", direction = "EAST",
                                 values = "[1.1235 ... 35]", unit = "m"))
  expect_identical(CoordinatesIrregular$new("c", "EAST", "", c(1, 5))$brief()$unit, "-")
})

test_that("CoordinatesIrregular copy() is a deep, equal copy", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35), bounds = c(-1, 2), attributes = list(a = 1))
  cp <- crd$copy()
  expect_false(identical(crd, cp))
  expect_identical(cp$values, crd$values)
  expect_identical(cp$bounds_raw, crd$bounds_raw)
  expect_identical(cp$attributes, crd$attributes)
  cp$name <- "other"
  expect_identical(crd$name, "c")
})

test_that("CoordinatesIrregular subset() keeps irregular values unpacked", {
  b <- rbind(c(5, 15, 30, 45), c(15, 30, 40, 60))
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35, 50), bounds = b)
  s <- crd$subset(c(2L, 3L))
  expect_s3_class(s, "CoordinatesIrregular")
  expect_false(inherits(s, "CoordinatesPacked"))
  expect_identical(s$values, c(20, 35))
  expect_identical(s$bounds, b[, 2:3])
})

test_that("CoordinatesIrregular subset() packs a regular result", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(3, 5, 7, 12), bounds = c(-1, 1))
  s <- crd$subset(c(1L, 3L))
  expect_s3_class(s, "CoordinatesPacked")
  expect_identical(s$raw, c(3, 2))
  expect_identical(s$values, c(3, 5, 7))
  expect_identical(s$bounds_raw, c(-1, 1))
})

test_that("CoordinatesIrregular subset() packs a regular result starting at its increment", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(2, 4, 6, 9))
  expect_identical(crd$subset(c(1L, 3L))$values, c(2, 4, 6))
})

test_that("Coordinates slice() without boundaries", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35, 50))
  expect_identical(crd$slice(c(15, 40)), c(2L, 3L))
  expect_identical(crd$slice(c(40, 15)), c(2L, 3L))
  expect_identical(crd$slice(c(0, 100)), c(1L, 4L))
  expect_identical(crd$slice(20), c(2L, 2L))
  expect_null(crd$slice(c(100, 200)))
})

test_that("Coordinates slice() with boundaries includes partial overlap", {
  crd <- CoordinatesIrregular$new("c", "EAST", "m", c(10, 20, 35), bounds = rbind(c(5, 15, 30), c(15, 30, 40)))
  expect_identical(crd$slice(c(29, 31)), c(2L, 3L))
  expect_identical(crd$slice(c(16, 17)), c(2L, 2L))
  expect_null(crd$slice(c(41, 50)))
})

# CoordinatesPacked -------------------------------------------------------------

test_that("CoordinatesPacked unpacks values", {
  crd <- CoordinatesPacked$new("c", "EAST", "m", c(10, 2.5), 5L)
  expect_identical(crd$raw, c(10, 2.5))
  expect_identical(crd$values, c(10, 12.5, 15, 17.5, 20))
  expect_identical(crd$length, 5L)
  expect_identical(crd$range, c(10, 20))
})

test_that("CoordinatesPacked with decreasing increment", {
  crd <- CoordinatesPacked$new("c", "NORTH", "m", c(90, -10), 4L)
  expect_identical(crd$values, c(90, 80, 70, 60))
  expect_identical(crd$range, c(90, 60))           # first, last
  expect_identical(crd$brief()$values, "[60 ... 90]") # sorted
})

test_that("CoordinatesPacked accepts start equal to increment", {
  crd <- CoordinatesPacked$new("c", "EAST", "m", c(1, 1), 3L)
  expect_identical(crd$values, c(1, 2, 3))
})

test_that("CoordinatesPacked copy()", {
  crd <- CoordinatesPacked$new("c", "EAST", "m", c(10, 2), 5L, bounds = c(-1, 1), attributes = list(a = 1))
  cp <- crd$copy()
  expect_s3_class(cp, "CoordinatesPacked")
  expect_identical(cp$values, crd$values)
  expect_identical(cp$bounds_raw, c(-1, 1))
  expect_identical(cp$attributes, list(a = 1))
})

test_that("CoordinatesPacked subset()", {
  crd <- CoordinatesPacked$new("c", "EAST", "m", c(10, 2), 5L, bounds = c(-1, 1))
  s <- crd$subset(c(2L, 4L))
  expect_s3_class(s, "CoordinatesPacked")
  expect_identical(s$values, c(12, 14, 16))
  expect_identical(s$bounds, rbind(c(11, 13, 15), c(13, 15, 17)))
})

test_that("CoordinatesPacked subset() with irregular boundary matrix", {
  b <- rbind(c(9, 11, 13, 15, 16), c(11, 13, 15, 17, 19))
  crd <- CoordinatesPacked$new("c", "EAST", "m", c(10, 2), 5L, bounds = b)
  expect_identical(crd$subset(c(2L, 3L))$bounds, b[, 2:3])
})

test_that("CoordinatesPacked subset() of a single element keeps a boundary matrix", {
  b <- rbind(c(9, 11, 13, 15, 16), c(11, 13, 15, 17, 19))
  crd <- CoordinatesPacked$new("c", "EAST", "m", c(10, 2), 5L, bounds = b)
  expect_identical(crd$subset(c(5L, 5L))$bounds, b[, 5L, drop = FALSE])
})

test_that("CoordinatesPacked slice()", {
  crd <- CoordinatesPacked$new("c", "EAST", "m", c(10, 2), 5L)
  expect_identical(crd$slice(c(13, 17)), c(3L, 4L))
  expect_null(crd$slice(c(100, 200)))
})

# CoordinatesString -------------------------------------------------------------

test_that("CoordinatesString basics", {
  crd <- CoordinatesString$new("cls", "OTHER", "-", c("a", "b", "c"), attributes = list(x = 1))
  expect_identical(crd$values, c("a", "b", "c"))
  expect_identical(crd$length, 3L)
  expect_identical(crd$brief()$values, "[a ... c]")
  expect_identical(crd$attributes, list(x = 1))
})

test_that("CoordinatesString copy() and subset()", {
  crd <- CoordinatesString$new("cls", "OTHER", "-", c("a", "b", "c", "d"), attributes = list(x = 1))
  cp <- crd$copy()
  expect_s3_class(cp, "CoordinatesString")
  expect_identical(cp$values, crd$values)
  s <- crd$subset(c(2L, 3L))
  expect_s3_class(s, "CoordinatesString")
  expect_identical(s$values, c("b", "c"))
  expect_identical(s$attributes, list(x = 1))
})

test_that("CoordinatesString accepts unsorted labels", {
  expect_identical(CoordinatesString$new("band", "OTHER", "-", c("red", "green"))$values,
                   c("red", "green"))
})

# CoordinatesTime ---------------------------------------------------------------

make_time <- function(...) CoordinatesTime$new("t", "FUTURE", "days", "2020-01-01", values = 0:9, ...)

test_that("CoordinatesTime formats values and exposes offsets", {
  crd <- make_time()
  expect_identical(crd$values[c(1, 10)], c("2020-01-01", "2020-01-10"))
  expect_equal(crd$offsets, 0:9)
  expect_identical(crd$length, 10L)
  expect_identical(crd$range, c("2020-01-01", "2020-01-10"))
  expect_s3_class(crd$time, "CFTime")
  expect_identical(crd$time$calendar$name, "standard")
})

test_that("CoordinatesTime with a model calendar", {
  crd <- CoordinatesTime$new("t", "FUTURE", "days", "2000-01-01", "360_day", values = c(29, 30))
  expect_identical(crd$values, c("2000-01-30", "2000-02-01"))
})

test_that("CoordinatesTime rejects an invalid definition", {
  expect_error(CoordinatesTime$new("t", "FUTURE", "fortnights", "2020-01-01", values = 0:2),
               "valid calendar definition")
  expect_error(CoordinatesTime$new("t", "FUTURE", "days", "2020-01-01", "no_calendar", values = 0:2),
               "valid calendar definition")
})

test_that("CoordinatesTime packed boundaries", {
  crd <- make_time(bounds = c(-0.5, 0.5))
  expect_identical(crd$bounds_raw, c(-0.5, 0.5))
  expect_identical(crd$bounds[, 1:2], rbind(c(-0.5, 0.5), c(0.5, 1.5)))
  expect_false(is.null(crd$time$bounds))
})

test_that("CoordinatesTime boundary matrix", {
  b <- rbind(0:9, 1:10)
  crd <- make_time(bounds = b)
  expect_equal(crd$bounds_raw, c(0, 1)) # regular, so packed
  b[2, 10] <- 12
  crd <- make_time(bounds = b)
  expect_identical(crd$bounds_raw, b)
})

test_that("CoordinatesTime copy()", {
  crd <- CoordinatesTime$new("t", "FUTURE", "hours", "2020-01-01", "noleap", values = c(0, 6, 24),
                             bounds = c(-3, 3), attributes = list(a = 1))
  cp <- crd$copy()
  expect_s3_class(cp, "CoordinatesTime")
  expect_identical(cp$values, crd$values)
  expect_identical(cp$time$calendar$name, "noleap")
  expect_identical(cp$bounds_raw, c(-3, 3))
  expect_identical(cp$attributes, list(a = 1))
})

test_that("CoordinatesTime subset()", {
  crd <- make_time()
  s <- crd$subset(c(3L, 5L))
  expect_s3_class(s, "CoordinatesTime")
  expect_identical(s$values, c("2020-01-03", "2020-01-04", "2020-01-05"))
  expect_null(s$bounds_raw)
})

test_that("CoordinatesTime subset() with boundaries", {
  crd <- make_time(bounds = c(0, 1))
  s <- crd$subset(c(3L, 5L))
  expect_s3_class(s, "CoordinatesTime")
  expect_identical(s$values, c("2020-01-03", "2020-01-04", "2020-01-05"))
  expect_equal(s$bounds_raw, c(0, 1))
})

test_that("CoordinatesTime slice() by full date", {
  crd <- make_time()
  expect_identical(crd$slice("2020-01-03"), c(3L, 3L))
  expect_identical(crd$slice(c("2020-01-03", "2020-01-06")), c(3L, 5L))
  expect_identical(crd$slice(c("2020-01-03", "2020-01-06"), rightmost.closed = TRUE), c(3L, 6L))
  expect_null(crd$slice(c("2021-01-01", "2021-02-01")))
})

test_that("CoordinatesTime slice() by year and month", {
  crd <- CoordinatesTime$new("t", "FUTURE", "days", "2019-12-01", values = seq(0, 90, by = 15))
  # 2019-12-01, -16, -31, 2020-01-15, -30, 2020-02-14, 2020-02-29
  expect_identical(crd$slice(2020), c(4L, 7L))
  expect_identical(crd$slice(c(2019, 2019)), c(1L, 3L))
  expect_identical(crd$slice("2019"), c(1L, 3L))
  expect_identical(crd$slice("2020-01"), c(4L, 5L))
  expect_identical(crd$slice(c("2019-12", "2020-01")), c(1L, 5L))
})

test_that("CoordinatesTime slice() by quarter, season and dekad", {
  crd <- CoordinatesTime$new("t", "FUTURE", "days", "2019-12-01", values = seq(0, 90, by = 15))
  expect_identical(crd$slice("2020-Q1"), c(4L, 7L))
  expect_identical(crd$slice("2019-Q4"), c(1L, 3L))
  expect_identical(crd$slice("2020-S1"), c(1L, 7L)) # DJF: 2019-12 to 2020-02
  expect_identical(crd$slice("2020-D02"), c(4L, 4L)) # 2020-01-11 to 2020-01-20
})

test_that("CoordinatesTime slice() by the last dekad of the year", {
  crd <- CoordinatesTime$new("t", "FUTURE", "days", "2019-12-01", values = seq(0, 90, by = 15))
  expect_identical(crd$slice("2019-D36"), c(3L, 3L)) # 2019-12-21 to 2020-01-01
})

test_that("CoordinatesTime slice() rejects bad timestamps", {
  crd <- make_time()
  expect_error(crd$slice(TRUE), "Bad format")
  expect_error(crd$slice(c("2020-01-01", "2020-01-02", "2020-01-03")), "Bad format")
  expect_error(crd$slice("2020-02-30"), "not valid in calendar")
})

# CoordinatesOrdinal ------------------------------------------------------------

test_that("CoordinatesOrdinal is 0-based by default", {
  crd <- CoordinatesOrdinal$new("o", "OTHER", 4L)
  expect_s3_class(crd, "CoordinatesPacked")
  expect_identical(crd$values, 0:3)
  expect_identical(crd$unit, "-")
  expect_identical(CoordinatesOrdinal$new("o", "OTHER", 3L, low = 5L)$values, 5:7)
})

test_that("CoordinatesOrdinal validates length", {
  expect_error(CoordinatesOrdinal$new("o", "OTHER", 4), "positive integer")
  expect_error(CoordinatesOrdinal$new("o", "OTHER", 0L), "positive integer")
  expect_error(CoordinatesOrdinal$new("o", "OTHER", 1:2), "positive integer")
})

test_that("CoordinatesOrdinal copy()", {
  cp <- CoordinatesOrdinal$new("o", "OTHER", 3L, low = 5L, attributes = list(a = 1))$copy()
  expect_s3_class(cp, "CoordinatesOrdinal")
  expect_identical(cp$values, 5:7)
  expect_identical(cp$attributes, list(a = 1))
})

test_that("CoordinatesOrdinal subset() keeps the ordinal values of the selected indices", {
  crd <- CoordinatesOrdinal$new("o", "OTHER", 5L)            # 0 1 2 3 4
  expect_identical(crd$subset(c(2L, 3L))$values, 1:2)
  crd <- CoordinatesOrdinal$new("o", "OTHER", 5L, low = 10L) # 10 .. 14
  expect_identical(crd$subset(c(2L, 3L))$values, 11:12)
})

# .make_coordinate_values() -----------------------------------------------------

test_that(".make_coordinate_values() packs regular numeric values", {
  expect_identical(gz$.make_coordinate_values(c(1L, 3L, 5L)),
                   list(mode = "integer", values = c(1L, 2L), length = 3L))
  expect_identical(gz$.make_coordinate_values(c(0.5, 1, 1.5)),
                   list(mode = "double", values = c(0.5, 0.5), length = 3L))
})

test_that(".make_coordinate_values() keeps short and irregular values", {
  expect_identical(gz$.make_coordinate_values(c(1, 2)), list(mode = "double", values = c(1, 2)))
  expect_identical(gz$.make_coordinate_values(c(1, 2, 5)), list(mode = "double", values = c(1, 2, 5)))
  expect_identical(gz$.make_coordinate_values(c("a", "b", "c")),
                   list(mode = "character", values = c("a", "b", "c")))
  expect_null(gz$.make_coordinate_values(c(TRUE, FALSE, TRUE)))
})

test_that(".make_coordinate_values() keeps irregular integer values", {
  expect_identical(gz$.make_coordinate_values(c(1L, 2L, 5L)),
                   list(mode = "integer", values = c(1L, 2L, 5L)))
})
