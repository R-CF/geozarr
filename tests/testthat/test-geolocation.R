# Geolocation arrays: CoordinateSystem field, set_convention(), reading, resolving

geo_axes <- function() list(
  x = CoordinateSystemAxis$new("x", "X", CoordinatesPacked$new("x_c", "EAST", "m", c(15, 10), 4L)),
  y = CoordinateSystemAxis$new("y", "Y", CoordinatesPacked$new("y_c", "NORTH", "m", c(75, -10), 3L))
)
geo_conv <- function(kind = "geodetic", id = NULL) {
  g <- zarr_convention_geolocation$new()
  g$set(kind, "../lon", "../lat", id = id)
}
conv_names <- function(atts) vapply(atts$zarr_conventions, function(c) c$name, character(1L))

# CoordinateSystem --------------------------------------------------------------

test_that("CoordinateSystem geolocation defaults to NULL", {
  expect_null(CoordinateSystem$new("cs", geo_axes())$geolocation)
})

test_that("CoordinateSystem geolocation can be set and removed", {
  cs <- CoordinateSystem$new("cs", geo_axes())
  g <- geo_conv()
  cs$geolocation <- g
  expect_identical(cs$geolocation, g)
  cs$geolocation <- NULL
  expect_null(cs$geolocation)
})

test_that("CoordinateSystem geolocation must be a set geolocation convention", {
  cs <- CoordinateSystem$new("cs", geo_axes())
  expect_error(cs$geolocation <- list(geodetic = 1), "zarr_convention_geolocation")
  expect_error(cs$geolocation <- zarr_convention_geolocation$new(), "`set\\(\\)`")
})

test_that("CoordinateSystem geolocation requires one X and one Y axis", {
  axes <- geo_axes()
  cs <- CoordinateSystem$new("cs", axes["x"])
  expect_error(cs$geolocation <- geo_conv(), 'one "X" axis')
  axes$y2 <- CoordinateSystemAxis$new("y2", "Y", CoordinatesPacked$new("y2_c", "NORTH", "m", c(0, 1), 2L))
  cs <- CoordinateSystem$new("cs", axes)
  expect_error(cs$geolocation <- geo_conv(), 'one "Y" axis')
})

# set_convention() --------------------------------------------------------------

test_that("set_convention() writes geolocation at the top level with spatial", {
  cs <- CoordinateSystem$new("cs", geo_axes())
  cs$geolocation <- geo_conv()
  atts <- set_convention(list(shape = c(4L, 3L)), cs)$attributes
  expect_identical(conv_names(atts), c("spatial", "geolocation", "ref"))
  expect_identical(atts$geolocation, cs$geolocation$as_list())
})

test_that("set_convention() nests geolocation in the planar CRS with cs", {
  axes <- geo_axes()
  axes$t <- CoordinateSystemAxis$new("t", "T", CoordinatesTime$new("t_c", "FUTURE", "days", "2020-01-01", values = c(0, 1)))
  cs <- CoordinateSystem$new("cs", axes)
  cs$geolocation <- geo_conv(id = zarr_conv_proj$new(list(code = "EPSG:4326")))
  atts <- set_convention(list(shape = c(4L, 3L, 2L)), cs)$attributes
  expect_true(all(c("cs", "geolocation", "ref", "proj") %in% conv_names(atts)))
  expect_null(atts$geolocation)
  types <- vapply(atts$cs$crs, function(c) c$type, character(1L))
  expect_identical(atts$cs$crs[[which(types == "planar")]]$geolocation, cs$geolocation$as_list())
  expect_null(atts$cs$crs[[which(types == "temporal")]]$geolocation)
})

test_that("set_convention() removes a previous geolocation attribute", {
  meta <- list(shape = c(4L, 3L), attributes = list(geolocation = list(foo = 1)))
  expect_null(set_convention(meta, CoordinateSystem$new("cs", geo_axes()))$attributes$geolocation)
})

# Reading and resolving ---------------------------------------------------------

# Store with array "a" carrying `atts`, plus "lon" and "lat" geolocation arrays
# with dimensions `geo_dims`. Array "a" has dimensions x, y (4 x 3) and, with
# `a_time = TRUE`, a third dimension t of length 2. Arguments `lon_dims` and
# `lat_shape` build deficient arrays.
geo_store <- function(atts, geo_dims = c("x", "y"), lon_dims = geo_dims,
                      lat_shape = c(x = 4L, y = 3L, t = 2L)[geo_dims], a_time = FALSE) {
  fn <- tempfile(fileext = ".zarr")
  root <- create_zarr(fn)[["/"]]
  a_shp <- c(x = 4L, y = 3L, t = 2L)[if (a_time) 1:3 else 1:2]
  write_plain_array(root, array(as.numeric(seq_len(prod(a_shp))), a_shp), "a", names(a_shp), atts)
  shp <- c(x = 4L, y = 3L, t = 2L)[geo_dims]
  write_plain_array(root, array(as.numeric(seq_len(prod(shp))), shp), "lon", lon_dims)
  write_plain_array(root, array(0, lat_shape), "lat", geo_dims)
  fn
}

spatial_geo_atts <- function(geo = geo_conv(), declare = TRUE) {
  atts <- zarr_conv_spatial$new()$register(list())
  if (declare) atts <- geo$register(atts)
  c(atts, list(`spatial:dimensions` = c("y", "x"),
               `spatial:transform` = c(10, 0, 15, 0, -10, 75),
               geolocation = geo$as_list()))
}

cs_geo_atts <- function(geo = geo_conv()) {
  csb <- zarr_convention_cs$new()
  x <- csb$axis(list(csb$coordinates(csb$values_regular(15, 10))), "X")
  y <- csb$axis(list(csb$coordinates(csb$values_regular(55, 10))), "Y")
  csb$add_crs(list(x = x, y = y), type = "planar", geolocation = geo)
  c(list(cs = csb$as_list()), geo$register(csb$register(list())))
}

test_that("spatial array reads its geolocation", {
  a <- open_zarr(geo_store(spatial_geo_atts()))[["/a"]]
  expect_identical(a$coordinate_system$geolocation$as_list(), geo_conv()$as_list())
})

test_that("cs array reads its geolocation from the planar CRS", {
  a <- open_zarr(geo_store(cs_geo_atts()))[["/a"]]
  expect_identical(a$coordinate_system$geolocation$as_list(), geo_conv()$as_list())
})

test_that("geolocation is ignored when the convention is not declared", {
  a <- open_zarr(geo_store(spatial_geo_atts(declare = FALSE)))[["/a"]]
  expect_null(a$coordinate_system$geolocation)
})

test_that("geolocation() resolves the x and y arrays", {
  a <- open_zarr(geo_store(spatial_geo_atts()))[["/a"]]
  g <- a$geolocation()
  expect_identical(names(g), c("x", "y"))
  expect_identical(g$x$name, "lon")
  expect_identical(g$y$name, "lat")
})

test_that("geolocation() returns NULL for an absent kind", {
  a <- open_zarr(geo_store(spatial_geo_atts()))[["/a"]]
  expect_null(a$geolocation("planar"))
  expect_null(open_zarr(geo_store(spatial_geo_atts(declare = FALSE)))[["/a"]]$geolocation())
})

test_that("geolocation() accepts arrays with transposed dimensions", {
  a <- open_zarr(geo_store(spatial_geo_atts(), geo_dims = c("y", "x")))[["/a"]]
  expect_identical(a$geolocation()$x$shape, c(3L, 4L))
})

test_that("geolocation() checks dimension names of the arrays", {
  a <- open_zarr(geo_store(spatial_geo_atts(), lon_dims = c("col", "row")))[["/a"]]
  expect_error(a$geolocation(), '"../lon" must have dimensions "x" and "y"')
})

test_that("geolocation() checks lengths of the arrays", {
  a <- open_zarr(geo_store(spatial_geo_atts(), lat_shape = c(4L, 2L)))[["/a"]]
  expect_error(a$geolocation(), '"../lat" does not match the lengths')
})

test_that("geolocation() accepts an additional dimension of the array", {
  a <- open_zarr(geo_store(spatial_geo_atts(), geo_dims = c("t", "x", "y"), a_time = TRUE))[["/a"]]
  expect_identical(a$geolocation()$y$shape, c(2L, 4L, 3L))
})

test_that("geolocation() rejects a dimension that the array does not have", {
  a <- open_zarr(geo_store(spatial_geo_atts(), geo_dims = c("x", "y", "t")))[["/a"]]
  expect_error(a$geolocation(), '"../lon" has dimensions that are not in the array')
})

test_that("geolocation() checks the length of an additional dimension", {
  a <- open_zarr(geo_store(spatial_geo_atts(), geo_dims = c("x", "y", "t"),
                           lat_shape = c(4L, 3L, 5L), a_time = TRUE))[["/a"]]
  expect_error(a$geolocation(), '"../lat" does not match the lengths')
})

test_that("geolocation() reports a missing array", {
  g <- zarr_convention_geolocation$new()
  g$set("geodetic", "../nope", "../lat")
  expect_error(open_zarr(geo_store(spatial_geo_atts(g)))[["/a"]]$geolocation(), '"../nope" not found')
})

test_that("cs with the same geolocation kind on two CRS objects is an error", {
  atts <- cs_geo_atts()
  atts$cs$crs[[2L]] <- atts$cs$crs[[1L]]
  names(atts$cs$crs[[2L]]$axes) <- c("u", "v")
  expect_error(open_zarr(geo_store(atts)), 'more than one "geodetic" geolocation')
})

test_that("subset() drops the geolocation", {
  a <- open_zarr(geo_store(spatial_geo_atts()))[["/a"]]
  expect_null(a$subset(X = c(10, 30))$coordinate_system$geolocation)
})
