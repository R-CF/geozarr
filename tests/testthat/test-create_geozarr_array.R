# create_geozarr_array()

X  <- c(15, 25, 35)   # regular, start != increment
Yd <- c(75, 65, 55)   # regular, decreasing
Yu <- c(5, 6, 8)      # increasing, irregular

new_group <- function(path = NULL) {
  z <- if (is.null(path)) create_zarr() else create_zarr(path)
  list(z = z, grp = z$add_group("/", "g"))
}

test_that("create_geozarr_array() validates its arguments", {
  g <- new_group()$grp
  expect_error(create_geozarr_array("a", "/g", list(), "int8"), "zarr_group instance")
  expect_error(create_geozarr_array("a", g, 1, "int8"), "list with axis descriptions")
  expect_error(create_geozarr_array("a", g, list(list(coordinates = X)), "int8"), "named elements")
  expect_error(create_geozarr_array("a", g, list(x = list(coordinates = X, abbreviation = "Q")), "int8"),
               'Bad abbreviation for axis "x"')
  expect_error(create_geozarr_array("a", g, list(x = list(coordinates = X), lon = list(coordinates = X)), "int8"),
               "Duplicate axis abbreviations")
})

test_that("create_geozarr_array() makes a spatial array", {
  s <- new_group()
  gza <- create_geozarr_array("a", s$grp, list(x = list(coordinates = X), y = list(coordinates = Yd)),
                              "int16", fill_value = -1L)
  expect_s3_class(gza, "geozarr_array")
  expect_identical(s$z$arrays, "/g/a")
  expect_identical(gza$shape, c(3L, 3L))
  expect_identical(gza$metadata$fill_value, -1L)
  expect_identical(gza$metadata$attributes$zarr_conventions[[1L]]$name, "spatial")
  expect_identical(gza$metadata$attributes[["spatial:registration"]], "pixel")
  expect_identical(axis_names(gza), c("x", "y"))
})

test_that("create_geozarr_array() passes the spatial registration", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(x = list(coordinates = X), y = list(coordinates = Yd),
                                   .convention = list(registration = "node")), "int16")
  expect_identical(gza$metadata$attributes[["spatial:registration"]], "node")
})

test_that("create_geozarr_array() infers abbreviation and direction", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(lon = list(coordinates = X), lat = list(coordinates = Yu),
                                   h = list(coordinates = 2.5, abbreviation = "Z")), "float32")
  axes <- gza$coordinate_system$axes
  expect_identical(vapply(axes, function(a) a$abbreviation, ""), c(lon = "X", lat = "Y", h = "Z"))
  expect_identical(vapply(axes, function(a) a$coordinates$direction, ""), c(lon = "EAST", lat = "NORTH", h = "OTHER"))
  expect_identical(create_geozarr_array("b", new_group()$grp,
                                        list(x = list(coordinates = X, direction = "WEST"), y = list(coordinates = Yu)),
                                        "int8")$coordinate_system$axes$x$coordinates$direction, "WEST")
})

test_that("create_geozarr_array() maps heuristic 'other' axis names", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(x = list(coordinates = X), y = list(coordinates = Yu),
                                   band = list(coordinates = c(450, 550, 650))), "int8")
  expect_identical(gza$coordinate_system$axes$band$abbreviation, "OTHER")
})

test_that("create_geozarr_array() with units and boundaries", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(x = list(coordinates = X, unit = "m", boundaries = c(-5, 5)),
                                   y = list(coordinates = Yu, unit = "m")), "float64")
  crd <- gza$coordinate_system$axes$x$coordinates
  expect_identical(crd$unit, "m")
  expect_identical(crd$bounds_raw, c(-5, 5))
  expect_identical(gza$metadata$attributes$cs$crs[[1L]]$axes$x$coordinates[[1L]]$boundaries,
                   list(regular = c(-5, 5)))
})

test_that("create_geozarr_array() with numeric time offsets", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(x = list(coordinates = X), y = list(coordinates = Yu),
                                   t = list(coordinates = c(0, 1, 3), unit = "days",
                                            calendar = "360_day", epoch = "2000-01-01")), "float32")
  crd <- gza$coordinate_system$axes$t$coordinates
  expect_s3_class(crd, "CoordinatesTime")
  expect_identical(crd$values, c("2000-01-01", "2000-01-02", "2000-01-04"))
  expect_identical(crd$time$calendar$name, "360_day")
})

test_that("create_geozarr_array() with character time stamps and a unit", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(x = list(coordinates = X), y = list(coordinates = Yu),
                                   time = list(coordinates = c("2020-01-01", "2020-01-02"), unit = "days")),
                              "float32")
  expect_s3_class(gza$coordinate_system$axes$time$coordinates, "CoordinatesTime")
})

test_that("create_geozarr_array() with character time stamps and no unit", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(x = list(coordinates = X), y = list(coordinates = Yu),
                                   time = list(coordinates = c("2020-01-01", "2020-01-02"))),
                              "float32")
  expect_identical(gza$coordinate_system$axes$time$coordinates$values, c("2020-01-01", "2020-01-02"))
})

test_that("create_geozarr_array() with string coordinates", {
  gza <- create_geozarr_array("a", new_group()$grp,
                              list(x = list(coordinates = X), y = list(coordinates = Yu),
                                   cls = list(coordinates = c("forest", "grass", "water"), abbreviation = "OTHER")),
                              "int8")
  expect_s3_class(gza$coordinate_system$axes$cls$coordinates, "CoordinatesString")
})

test_that("create_geozarr_array() writes external coordinates and bounds", {
  local_geozarr_option("max_explicit", 2L)
  s <- new_group()
  gza <- create_geozarr_array("a", s$grp,
                              list(x = list(coordinates = c(1, 2, 4), boundaries = rbind(c(.5, 1.5, 3), c(1.5, 3, 5))),
                                   y = list(coordinates = Yu)), "float64")
  expect_setequal(s$z$arrays, c("/g/a", "/g/x", "/g/x_bounds", "/g/y"))
  expect_identical(s$z[["/g/x"]][], c(1, 2, 4))
  expect_identical(s$z[["/g/x_bounds"]][], rbind(c(.5, 1.5, 3), c(1.5, 3, 5)))
})

test_that("create_geozarr_array() writes externals into another group", {
  local_geozarr_option("max_explicit", 2L)
  s <- new_group()
  s$z$add_group("/", "coords")
  create_geozarr_array("a", s$grp, list(x = list(coordinates = X), y = list(coordinates = Yu),
                                        .convention = list(coordinate_group = "../../coords")), "float64")
  expect_setequal(s$z$arrays, c("/coords/y", "/g/a"))
})

test_that("create_geozarr_array() shares compatible external coordinates", {
  local_geozarr_option("max_explicit", 2L)
  s <- new_group()
  axes <- list(x = list(coordinates = X), y = list(coordinates = Yu))
  create_geozarr_array("a", s$grp, axes, "float64")
  expect_no_error(create_geozarr_array("b", s$grp, axes, "float64"))
  expect_setequal(s$z$arrays, c("/g/a", "/g/b", "/g/y"))
})

test_that("create_geozarr_array() refuses incompatible external coordinates", {
  local_geozarr_option("max_explicit", 2L)
  s <- new_group()
  create_geozarr_array("a", s$grp, list(x = list(coordinates = X), y = list(coordinates = Yu)), "float64")
  expect_error(create_geozarr_array("b", s$grp, list(x = list(coordinates = X), y = list(coordinates = c(5, 7, 8))),
                                    "float64"),
               'Incompatible external array "y"')
})

test_that("create_geozarr_array() puts units and dimension names on the external array", {
  local_geozarr_option("max_explicit", 2L)
  s <- new_group()
  create_geozarr_array("a", s$grp, list(x = list(coordinates = X), y = list(coordinates = Yu, unit = "m")), "float64")
  expect_identical(s$z[["/g/y"]]$metadata$dimension_names, "y")
  expect_identical(s$z[["/g/y"]]$attribute("units"), "m")
  expect_null(s$z[["/g"]]$metadata$dimension_names)
})
