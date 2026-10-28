# print() methods, on both the plain and the cli path

time_axis <- function() CoordinateSystemAxis$new("time", "T",
  CoordinatesTime$new("time_c", "FUTURE", "days", "2020-01-01", "noleap", values = c(0, 31, 59),
                      attributes = list(long_name = "Time")))

test_that("Coordinates print (plain)", {
  local_plain_theme()
  out <- capture.output(CoordinatesIrregular$new("lon_c", "EAST", "degrees_east", c(10, 20, 35))$print())
  expect_identical(out[1:4], c("<Coordinates> lon_c", "Length    : 3 ", "Direction : EAST ",
                               "Values    : [10 ... 35] degrees_east "))
  out <- capture.output(CoordinatesIrregular$new("h", "UP", "m", 2)$print())
  expect_identical(out[4], "Values    : [2] m ")
})

test_that("CoordinateSystemAxis print (plain)", {
  local_plain_theme()
  out <- capture.output(print(time_axis()))
  expect_identical(out[1:5], c("<Axis> time", "Abbreviation : T ", "Calendar     : noleap ",
                               "Epoch        : 2020-01-01T00:00:00 ", "Coordinates  : 3"))
  expect_true(any(grepl("2020-01-01 ... 2020-03-01", out, fixed = TRUE)))
  expect_true(any(grepl("long_name", out)))
})

test_that("CoordinateSystemAxis print lists every coordinate set", {
  local_plain_theme()
  ax <- CoordinateSystemAxis$new("x", "", list(a = CoordinatesIrregular$new("a", "EAST", "m", c(10, 20, 35)),
                                               b = CoordinatesString$new("b", "OTHER", "-", c("p", "q", "r"))),
                                 attributes = list(note = "two sets"))
  out <- capture.output(print(ax))
  expect_identical(out[2], "Abbreviation : - ")
  expect_true(any(grepl("^ *name", out)))
  expect_true(any(grepl("note", out)))
})

test_that("CoordinateSystem print (plain)", {
  local_plain_theme()
  cs <- CoordinateSystem$new("cs", list(time = time_axis()))
  out <- capture.output(print(cs))
  expect_identical(out[1], "<Coordinate System>")
  expect_true(any(grepl("noleap|FUTURE", out)))
})

test_that("geozarr_array print shows the coordinate system and hides convention attributes", {
  local_plain_theme()
  a <- array(1:24, c(2, 3, 4))
  dimnames(a) <- list(x = c(15, 25), y = c(5, 6, 8), time = sprintf("2026-01-%02d", 1:4))
  g <- as_geozarr(a)
  g$set_attribute("title", "demo")
  out <- capture.output(print(g))
  expect_true(any(grepl("Coordinate system", out)))
  expect_true(any(grepl("title", out)))
  expect_false(any(grepl("zarr_conventions", out)))
})

test_that("print methods work with the cli theme", {
  skip_if_not_installed("cli")
  expect_no_error(suppressMessages(print(time_axis())))
  expect_no_error(suppressMessages(print(time_axis()$coordinates)))
  expect_no_error(suppressMessages(print(CoordinateSystem$new("cs", list(time = time_axis())))))
  a <- array(1:6, c(2, 3)); dimnames(a) <- list(x = c(15, 25), y = c(5, 6, 8))
  expect_no_error(suppressMessages(print(as_geozarr(a))))
})
