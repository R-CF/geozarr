# set_convention(): choosing and writing the "spatial" or "cs" convention

ax <- function(name, abbr, crd) CoordinateSystemAxis$new(name, abbr, crd)
regx  <- function(n = 4L) ax("x", "X", CoordinatesPacked$new("x_c", "EAST", "m", c(15, 10), n))
regyd <- function(n = 3L) ax("y", "Y", CoordinatesPacked$new("y_c", "NORTH", "m", c(75, -10), n))
regyu <- function(n = 3L) ax("y", "Y", CoordinatesPacked$new("y_c", "NORTH", "m", c(55, 10), n))
make_cs <- function(...) {
  axes <- list(...)
  names(axes) <- vapply(axes, function(a) a$name, character(1L))
  CoordinateSystem$new("cs", axes)
}
conv_names <- function(meta) vapply(meta$attributes$zarr_conventions, function(c) c$name, character(1L))

# Convention selection ----------------------------------------------------------

test_that("set_convention() requires an X or Y axis", {
  cs <- make_cs(ax("band", "OTHER", CoordinatesString$new("b", "OTHER", "-", c("a", "b"))))
  expect_error(set_convention(list(), cs), "No X and/or Y axes")
})

test_that("set_convention() uses spatial for north-up regular X/Y", {
  m <- set_convention(list(shape = c(4L, 3L)), make_cs(regx(), regyd()))
  expect_identical(m$dimension_names, c("x", "y"))
  expect_identical(conv_names(m), "spatial")
  expect_identical(m$attributes[["spatial:dimensions"]], c("y", "x"))
  expect_identical(m$attributes[["spatial:transform"]], c(10, 0, 10, 0, -10, 80))
  expect_identical(m$attributes[["spatial:bbox"]], c(10, 50, 50, 80))
  expect_null(m$attributes$cs)
})

test_that("set_convention() spatial with a third, categorical axis", {
  band <- ax("band", "OTHER", CoordinatesString$new("b", "OTHER", "-", c("a", "b")))
  m <- set_convention(list(), make_cs(regx(), regyd(), band))
  expect_identical(conv_names(m), "spatial")
  expect_identical(m$dimension_names, c("x", "y", "band"))
})

test_that("set_convention() spatial honours the registration argument", {
  m <- set_convention(list(), make_cs(regx(), regyd()), registration = "node")
  expect_identical(m$attributes[["spatial:registration"]], "node")
})

test_that("set_convention() falls back to cs when spatial does not apply", {
  # Y increasing
  expect_identical(conv_names(set_convention(list(), make_cs(regx(), regyu()))), "cs")
  # Irregular X
  irrx <- ax("x", "X", CoordinatesIrregular$new("x_c", "EAST", "m", c(15, 25, 40, 60)))
  expect_identical(conv_names(set_convention(list(), make_cs(irrx, regyd()))), "cs")
  # A T axis
  t <- ax("t", "T", CoordinatesTime$new("t_c", "FUTURE", "days", "2020-01-01", values = c(0, 1)))
  expect_identical(conv_names(set_convention(list(), make_cs(regx(), regyd(), t))), "cs")
  # Only X
  expect_identical(conv_names(set_convention(list(), make_cs(regx()))), "cs")
})

# Existing attributes -----------------------------------------------------------

test_that("set_convention() replaces previous convention attributes", {
  meta <- list(dimension_names = c("a", "b"),
               attributes = list(title = "keep", `spatial:bbox` = 1:4, `proj:code` = "EPSG:1",
                                 cs = list(1), zarr_conventions = list(list(name = "old"))))
  m <- set_convention(meta, make_cs(regx(), regyu()))
  expect_identical(m$dimension_names, c("x", "y"))
  expect_identical(m$attributes$title, "keep")
  expect_null(m$attributes[["spatial:bbox"]])
  expect_null(m$attributes[["proj:code"]])
  expect_identical(conv_names(m), "cs")
  expect_identical(names(m$attributes)[1L], "cs")
})

# CRS argument ------------------------------------------------------------------

test_that("set_convention() spatial with a proj CRS", {
  m <- set_convention(list(), make_cs(regx(), regyd()), list(spatial = list(code = "EPSG:32631")))
  expect_identical(conv_names(m), c("spatial", "proj"))
  expect_identical(m$attributes[["proj:code"]], "EPSG:32631")
})

test_that("set_convention() spatial rejects a malformed CRS", {
  cs <- make_cs(regx(), regyd())
  expect_error(set_convention(list(), cs, list(vertical = list(code = "EPSG:5703"))), "formatted CRS")
  expect_error(set_convention(list(), cs, list(spatial = list(epsg = 1))), "formatted CRS")
  expect_error(set_convention(list(), cs, list(spatial = list(code = "EPSG:1"), compound = list(code = "EPSG:2"))),
               "formatted CRS")
  expect_error(set_convention(list(), cs, "EPSG:4326"), "formatted CRS")
})

test_that("set_convention() cs with per-CRS proj identifiers", {
  t <- ax("t", "T", CoordinatesTime$new("t_c", "FUTURE", "days", "2020-01-01", values = c(0, 1)))
  z <- ax("z", "Z", CoordinatesIrregular$new("z_c", "UP", "m", c(0, 10, 50)))
  m <- set_convention(list(), make_cs(regx(), regyu(), z, t),
                      list(spatial = list(code = "EPSG:4326"), vertical = list(code = "EPSG:5703")))
  expect_identical(conv_names(m), c("cs", "proj"))
  crs <- m$attributes$cs$crs
  expect_identical(vapply(crs, function(c) c$type, ""), c("planar", "vertical", "temporal"))
  expect_identical(crs[[1L]]$id, list(`proj:code` = "EPSG:4326"))
  expect_identical(crs[[2L]]$id, list(`proj:code` = "EPSG:5703"))
  expect_null(crs[[3L]]$id)
})

test_that("set_convention() cs with a compound CRS", {
  m <- set_convention(list(), make_cs(regx(), regyu()), list(compound = list(code = "EPSG:9518")))
  expect_identical(m$attributes$cs$id, list(`proj:code` = "EPSG:9518"))
})

# cs axis encoding --------------------------------------------------------------

cs_axes <- function(m) do.call(c, lapply(m$attributes$cs$crs, function(c) c$axes))

test_that("set_convention() cs groups axes by type", {
  t <- ax("t", "T", CoordinatesTime$new("t_c", "FUTURE", "days", "2020-01-01", values = c(0, 1)))
  m <- set_convention(list(), make_cs(t, regyu(), regx()))
  crs <- m$attributes$cs$crs
  expect_identical(vapply(crs, function(c) c$type, ""), c("planar", "temporal"))
  expect_identical(names(crs[[1L]]$axes), c("x", "y"))
  expect_identical(m$dimension_names, c("t", "y", "x"))
})

test_that("set_convention() cs encodes regular values and abbreviation/direction", {
  axes <- cs_axes(set_convention(list(), make_cs(regx(), regyu())))
  expect_identical(axes$x$abbreviation, "X")
  expect_identical(axes$x$direction, "EAST")
  expect_identical(axes$y$direction, "NORTH")
  expect_identical(axes$x$coordinates[[1L]]$values, list(regular = c(15, 10)))
  expect_identical(axes$x$coordinates[[1L]]$unit, "m")
})

test_that("set_convention() cs encodes short irregular values explicitly", {
  z <- ax("z", "Z", CoordinatesIrregular$new("z_c", "UP", "m", c(0, 10, 50)))
  axes <- cs_axes(set_convention(list(), make_cs(regx(), regyu(), z)))
  expect_identical(axes$z$coordinates[[1L]]$values, list(explicit = list(0, 10, 50)))
  expect_identical(axes$z$direction, "UP")
})

test_that("set_convention() cs refers long irregular values to an external array", {
  local_geozarr_option("max_explicit", 2L)
  z <- ax("z", "Z", CoordinatesIrregular$new("z_c", "UP", "m", c(0, 10, 50)))
  m <- set_convention(list(), make_cs(regx(), regyu(), z))
  expect_identical(cs_axes(m)$z$coordinates[[1L]]$values, list(external = list(ref = list(node = "../z"))))
  expect_true("ref" %in% conv_names(m))
  m <- set_convention(list(), make_cs(regx(), regyu(), z), external_group = "../coords")
  expect_identical(cs_axes(m)$z$coordinates[[1L]]$values$external$ref$node, "../coords/z")
})

test_that("set_convention() cs encodes boundaries", {
  zp <- ax("z", "Z", CoordinatesIrregular$new("z_c", "UP", "m", c(0, 10, 50), bounds = c(-1, 1)))
  expect_identical(cs_axes(set_convention(list(), make_cs(regx(), regyu(), zp)))$z$coordinates[[1L]]$boundaries,
                   list(regular = c(-1, 1)))
  zm <- ax("z", "Z", CoordinatesIrregular$new("z_c", "UP", "m", c(0, 10, 50), bounds = rbind(c(-5, 5, 30), c(5, 30, 70))))
  expect_identical(cs_axes(set_convention(list(), make_cs(regx(), regyu(), zm)))$z$coordinates[[1L]]$boundaries,
                   list(external = list(ref = list(node = "../z_bounds"))))
})

test_that("set_convention() cs encodes time coordinates", {
  treg <- ax("t", "T", CoordinatesTime$new("t_c", "FUTURE", "hours", "2020-01-01", "noleap", values = c(0, 6, 12)))
  crd <- cs_axes(set_convention(list(), make_cs(regx(), regyu(), treg)))$t$coordinates[[1L]]
  expect_identical(crd$values, list(regular = c(0, 6)))
  expect_identical(crd$time, list(unit = "hours", epoch = "2020-01-01", calendar = "noleap"))
  expect_null(crd$unit)
  expect_identical(cs_axes(set_convention(list(), make_cs(regx(), regyu(), treg)))$t$direction, "FUTURE")

  tirr <- ax("t", "T", CoordinatesTime$new("t_c", "FUTURE", "days", "2020-01-01", values = c(0, 31, 60)))
  crd <- cs_axes(set_convention(list(), make_cs(regx(), regyu(), tirr)))$t$coordinates[[1L]]
  expect_identical(crd$values, list(explicit = list(0, 31, 60)))
})

test_that("set_convention() cs encodes 'OTHER' axes without abbreviation", {
  t <- ax("t", "T", CoordinatesTime$new("t_c", "FUTURE", "days", "2020-01-01", values = c(0, 1)))
  band <- ax("band", "OTHER", CoordinatesString$new("b", "OTHER", "-", c("a", "b")))
  m <- set_convention(list(), make_cs(regx(), regyu(), t, band))
  crs <- m$attributes$cs$crs
  expect_identical(crs[[3L]]$type, "unknown")
  expect_identical(crs[[3L]]$axes$band$abbreviation, "")
  expect_identical(crs[[3L]]$axes$band$direction, "OTHER")
  expect_identical(crs[[3L]]$axes$band$coordinates[[1L]]$values, list(explicit = list("a", "b")))
})

test_that("set_convention() cs encodes axes with an empty abbreviation", {
  t <- ax("t", "T", CoordinatesTime$new("t_c", "FUTURE", "days", "2020-01-01", values = c(0, 1)))
  band <- ax("band", "", CoordinatesOrdinal$new("b", "OTHER", 2L))
  m <- set_convention(list(), make_cs(regx(), regyu(), t, band))
  expect_identical(m$attributes$cs$crs[[3L]]$axes$band$direction, "OTHER")
})
