# Geolocation data: CoordinateSystem$set_geolocation(), set_convention() and
# geozarr_array$write_geolocation()

wg_axes <- function(y_step = 1) list(
  rlon = CoordinateSystemAxis$new("rlon", "X", CoordinatesPacked$new("rlon_c", "EAST", "degrees", c(-10, 1), 4L)),
  rlat = CoordinateSystemAxis$new("rlat", "Y", CoordinatesPacked$new("rlat_c", "NORTH", "degrees", c(40, y_step), 3L))
)
wg_cs <- function(...) CoordinateSystem$new("cs", wg_axes(...))
lon <- array(-10 + 0:11 / 10, c(4L, 3L))
lat <- array(40 + 0:11 / 10, c(4L, 3L))
lon_atts <- list(units = "degrees_east", standard_name = "longitude")
lat_atts <- list(units = "degrees_north", standard_name = "latitude")

with_geo <- function(cs, x = lon, y = lat, dims = c("rlon", "rlat"), names = c("lon", "lat")) {
  cs$set_geolocation("geodetic", x, y, dims, names, id = zarr_conv_proj$new(list(code = "EPSG:4326")),
                     attributes = list(lon_atts, lat_atts))
}

# Store with group "/" (and "/geo"); writes array `name` with geolocation
# following the ncdfCF flow. Returns the new geozarr_array.
wg_write <- function(root, name = "pr", cs = with_geo(wg_cs()), ...) {
  meta <- set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs, ...)
  gza <- geozarr_array$new(name, meta, root, cs)
  gza$write(array(as.numeric(1:12), c(4L, 3L)))
  gza$write_geolocation()
}
wg_root <- function() {
  fn <- tempfile(fileext = ".zarr")
  root <- create_zarr(fn)[["/"]]
  root$add_group("geo")
  list(fn = fn, root = root)
}

# CoordinateSystem$set_geolocation() ------------------------------------------------

test_that("set_geolocation() holds the data until written", {
  cs <- with_geo(wg_cs())
  d <- cs$geolocation_data$geodetic
  expect_identical(c(d$x$name, d$y$name), c("lon", "lat"))
  expect_identical(d$dims, c("rlon", "rlat"))
  expect_identical(d$y$attributes, lat_atts)
  expect_null(cs$geolocation)
})

test_that("set_geolocation() accepts transposed data", {
  cs <- with_geo(wg_cs(), x = t(lon), y = t(lat), dims = c("rlat", "rlon"))
  expect_identical(cs$geolocation_data$geodetic$dims, c("rlat", "rlon"))
})

test_that("set_geolocation() requires X and Y axes", {
  cs <- CoordinateSystem$new("cs", wg_axes()["rlon"])
  expect_error(cs$set_geolocation("geodetic", lon[, 1], lat[, 1], "rlon", c("lon", "lat")), 'one "X" axis')
})

test_that("set_geolocation() checks dims against the axes", {
  cs <- wg_cs()
  expect_error(cs$set_geolocation("geodetic", lon[, 1], lat[, 1], "rlon", c("lon", "lat")),
               'must include the X and Y axes "rlon" and "rlat"')
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "time"), c("lon", "lat")),
               "must name axes of the coordinate system")
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlon"), c("lon", "lat")),
               "must name axes of the coordinate system")
})

test_that("set_geolocation() checks the shape of the data", {
  cs <- wg_cs()
  expect_error(cs$set_geolocation("geodetic", t(lon), lat, c("rlon", "rlat"), c("lon", "lat")),
               "matching the lengths of the axes")
  expect_error(cs$set_geolocation("geodetic", lon, array("a", c(4L, 3L)), c("rlon", "rlat"), c("lon", "lat")),
               "must be numeric arrays")
})

test_that("set_geolocation() checks names, id and attributes", {
  cs <- wg_cs()
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), "lon"), "`names`")
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lon")), "`names`")
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"), id = "EPSG:4326"),
               "zarr_conv_proj")
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"), attributes = list(lon_atts)),
               "list of two named lists")
  expect_error(cs$set_geolocation("polar", lon, lat, c("rlon", "rlat"), c("lon", "lat")), "should be one of")
})

test_that("set_geolocation() requires attributes as named lists", {
  cs <- wg_cs()
  df <- data.frame(name = "units", type = "NC_CHAR", length = 12L, value = "degrees_east")
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"), attributes = list(df, df)),
               "list of two named lists")
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"), attributes = list(list("x"), list())),
               "list of two named lists")
  expect_no_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"), attributes = list(list(), list())))
})

test_that("set_geolocation() replaces referenced geolocation of the same kind", {
  cs <- wg_cs()
  g <- zarr_convention_geolocation$new()
  cs$geolocation <- g$set("geodetic", "../a", "../b")$set("planar", "../e", "../n")
  with_geo(cs)
  expect_identical(names(cs$geolocation$as_list()), "planar")
})

test_that("setting the geolocation field drops data of the same kind", {
  cs <- with_geo(wg_cs())
  cs$geolocation <- zarr_convention_geolocation$new()$set("geodetic", "../a", "../b")
  expect_length(cs$geolocation_data, 0L)
})

# set_convention() ----------------------------------------------------------------

geo_of <- function(meta) {
  atts <- meta$attributes
  atts$geolocation %||% do.call(c, lapply(atts$cs$crs, function(c) c$geolocation))
}

test_that("set_convention() references geolocation data in the array's group", {
  meta <- set_convention(list(shape = c(4L, 3L)), with_geo(wg_cs()))
  g <- geo_of(meta)$geodetic
  expect_identical(c(g$x$values$ref$node, g$y$values$ref$node), c("../lon", "../lat"))
  expect_identical(g$id, list(`proj:code` = "EPSG:4326"))
})

test_that("set_convention() references geolocation data with the spatial convention", {
  meta <- set_convention(list(shape = c(4L, 3L)), with_geo(wg_cs(y_step = -1)))
  expect_false(is.null(meta$attributes[["spatial:transform"]]))
  expect_identical(meta$attributes$geolocation$geodetic$x$values$ref$node, "../lon")
})

test_that("set_convention() puts geolocation data in geolocation_group", {
  meta <- set_convention(list(shape = c(4L, 3L)), with_geo(wg_cs()),
                         external_group = "../coords", geolocation_group = "../geo")
  expect_identical(geo_of(meta)$geodetic$y$values$ref$node, "../geo/lat")
})

test_that("set_convention() falls back to external_group for geolocation data", {
  meta <- set_convention(list(shape = c(4L, 3L)), with_geo(wg_cs()), external_group = "../coords")
  expect_identical(geo_of(meta)$geodetic$x$values$ref$node, "../coords/lon")
})

# geozarr_array$write_geolocation() -----------------------------------------------------

test_that("write_geolocation() writes the arrays with dimensions and attributes", {
  s <- wg_root()
  wg_write(s$root)
  arr <- s$root$children$lat
  expect_identical(unlist(arr$metadata$dimension_names), c("rlon", "rlat"))
  expect_identical(arr$attribute("standard_name"), "latitude")
  expect_identical(arr$attribute("units"), "degrees_north")
  expect_equal(arr[], lat)
})

test_that("write_geolocation() frees the data and references the arrays", {
  s <- wg_root()
  gza <- wg_write(s$root)
  cs <- gza$coordinate_system
  expect_length(cs$geolocation_data, 0L)
  expect_identical(cs$geolocation$as_list()$geodetic$x$values$ref$node, "../lon")
})

test_that("geolocation() resolves right after writing", {
  s <- wg_root()
  expect_identical(wg_write(s$root)$geolocation()$y$name, "lat")
})

test_that("the written store has the same geolocation as the array in memory", {
  s <- wg_root()
  gza <- wg_write(s$root)
  reopened <- open_zarr(s$fn)[["/pr"]]
  expect_identical(reopened$coordinate_system$geolocation$as_list(),
                   gza$coordinate_system$geolocation$as_list())
  expect_equal(reopened$geolocation()$x[], lon)
})

test_that("write_geolocation() writes to geolocation_group", {
  s <- wg_root()
  wg_write(s$root, geolocation_group = "../geo")
  expect_false(is.null(s$root$children$geo$children$lon))
  expect_null(s$root$children$lon)
})

test_that("arrays sharing geolocation verify the existing arrays", {
  s <- wg_root()
  wg_write(s$root, "pr")
  expect_no_error(wg_write(s$root, "tas"))
  expect_identical(s$root$children$tas$geolocation()$x$name, "lon")
})

test_that("shared geolocation arrays with other values are an error", {
  s <- wg_root()
  wg_write(s$root, "pr")
  expect_error(wg_write(s$root, "tas", cs = with_geo(wg_cs(), x = lon + 1)), 'Incompatible external array "lon"')
})

test_that("shared geolocation arrays with other dimensions are an error", {
  s <- wg_root()
  wg_write(s$root, "pr", cs = with_geo(wg_cs(), x = t(lon), y = t(lat), dims = c("rlat", "rlon")))
  expect_error(wg_write(s$root, "tas"), 'Incompatible external array "lon"')
})

test_that("write_external_coordinates() also writes geolocation data", {
  s <- wg_root()
  cs <- with_geo(wg_cs())
  gza <- geozarr_array$new("pr", set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs), s$root, cs)
  gza$write_external_coordinates()
  expect_false(is.null(s$root$children$lon))
  expect_length(cs$geolocation_data, 0L)
})

test_that("write_geolocation() without geolocation data does nothing", {
  s <- wg_root()
  cs <- wg_cs()
  gza <- geozarr_array$new("pr", set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs), s$root, cs)
  expect_invisible(gza$write_geolocation())
  expect_null(s$root$children$lon)
})

test_that("write_geolocation() requires geolocation in the metadata", {
  s <- wg_root()
  cs <- wg_cs()
  gza <- geozarr_array$new("pr", set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs), s$root, cs)
  with_geo(cs)
  expect_error(gza$write_geolocation(), 'No "geodetic" geolocation in the metadata')
})

test_that("write_geolocation() rejects a single-array store", {
  cs <- with_geo(wg_cs())
  z <- zarr::zarr$new(tempfile(fileext = ".zarr"))
  gza <- geozarr_array$new("", set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs), z, cs)
  expect_error(gza$write_geolocation(), "Single-array Zarr store")
})

# Creating missing groups -----------------------------------------------------------

test_that("write_geolocation() fails on a missing group by default", {
  s <- wg_root()
  expect_error(wg_write(s$root, geolocation_group = "../coords/geo"), "non-existent child node")
})

test_that("write_geolocation(create_groups = TRUE) creates nested groups", {
  s <- wg_root()
  cs <- with_geo(wg_cs())
  meta <- set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs, geolocation_group = "../coords/geo")
  gza <- geozarr_array$new("pr", meta, s$root, cs)
  gza$write_geolocation(create_groups = TRUE)
  expect_false(is.null(s$root$children$coords$children$geo$children$lat))
})

test_that("groups created for geolocation are persisted", {
  s <- wg_root()
  cs <- with_geo(wg_cs())
  meta <- set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs, geolocation_group = "../coords/geo")
  gza <- geozarr_array$new("pr", meta, s$root, cs)
  gza$write(array(as.numeric(1:12), c(4L, 3L)))
  gza$write_geolocation(create_groups = TRUE)
  expect_equal(open_zarr(s$fn)[["/pr"]]$geolocation()$y[], lat)
})

test_that("create_groups cannot move beyond the root", {
  s <- wg_root()
  cs <- with_geo(wg_cs())
  meta <- set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs, geolocation_group = "../../geo")
  gza <- geozarr_array$new("pr", meta, s$root, cs)
  expect_error(gza$write_geolocation(create_groups = TRUE), "beyond the root node")
})

test_that("create_groups does not create a group inside an array", {
  s <- wg_root()
  wg_write(s$root, "tas")
  cs <- with_geo(wg_cs())
  meta <- set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs, geolocation_group = "../tas/geo")
  gza <- geozarr_array$new("pr", meta, s$root, cs)
  expect_error(gza$write_geolocation(create_groups = TRUE), "non-existent child node")
})

test_that("a geolocation group that is an array is an error", {
  s <- wg_root()
  wg_write(s$root, "tas")
  cs <- with_geo(wg_cs())
  meta <- set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs, geolocation_group = "../tas")
  gza <- geozarr_array$new("pr", meta, s$root, cs)
  expect_error(gza$write_geolocation(create_groups = TRUE), "is not a group")
})

test_that("write_external_coordinates(create_groups = TRUE) creates groups for coordinates and geolocation", {
  s <- wg_root()
  axes <- wg_axes()
  axes$rlon <- CoordinateSystemAxis$new("rlon", "X",
    CoordinatesIrregular$new("rlon_c", "EAST", "degrees", cumsum(c(0, seq_len(39) / 10))))
  cs <- with_geo(CoordinateSystem$new("cs", axes), x = array(0, c(40L, 3L)), y = array(0, c(40L, 3L)))
  meta <- set_convention(zarr::define_array("float64", c(40L, 3L))$metadata(), cs,
                         external_group = "../coords", geolocation_group = "../coords/geo")
  gza <- geozarr_array$new("pr", meta, s$root, cs)
  expect_error(gza$write_external_coordinates(), "non-existent child node")
  gza$write_external_coordinates(create_groups = TRUE)
  coords <- s$root$children$coords
  expect_false(is.null(coords$children$rlon))
  expect_false(is.null(coords$children$geo$children$lon))
})

# Boundaries -------------------------------------------------------------------------

lon_b <- array(seq_len(48) / 10, c(4L, 4L, 3L))  # nv, rlon, rlat
lat_b <- array(seq_len(48) / 20, c(4L, 4L, 3L))
b_dims <- c("nv", "rlon", "rlat")

with_bounds <- function(cs, ...) {
  cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"),
                     attributes = list(lon_atts, lat_atts),
                     x_bounds = lon_b, y_bounds = lat_b, bounds_dims = b_dims, ...)
}

test_that("set_geolocation() holds boundary data", {
  d <- with_bounds(wg_cs())$geolocation_data$geodetic
  expect_identical(c(d$x$bounds$name, d$y$bounds$name), c("lon_bnds", "lat_bnds"))
  expect_identical(d$bounds_dims, b_dims)
  expect_identical(d$y$bounds$values, lat_b)
})

test_that("set_geolocation() takes names for the boundary arrays", {
  d <- with_bounds(wg_cs(), bounds_names = c("lon_vertices", "lat_vertices"))$geolocation_data$geodetic
  expect_identical(d$x$bounds$name, "lon_vertices")
})

test_that("set_geolocation() requires boundaries for both x and y", {
  expect_error(wg_cs()$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"),
                                       x_bounds = lon_b, bounds_dims = b_dims),
               "both be given, or neither")
})

test_that("set_geolocation() checks bounds_dims", {
  cs <- wg_cs()
  f <- function(bd) cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"),
                                       x_bounds = lon_b, y_bounds = lat_b, bounds_dims = bd)
  expect_error(f(NULL), "`bounds_dims`")
  expect_error(f(c("rlon", "rlat")), "`bounds_dims`")
  expect_error(f(c("nv", "rlon", "rlat", "x")), "`bounds_dims`")
  expect_error(f(c("nv", "rlon", "nv")), "`bounds_dims`")
})

test_that("set_geolocation() rejects an axis as the vertex dimension", {
  axes <- wg_axes()
  axes$time <- CoordinateSystemAxis$new("time", "T", CoordinatesPacked$new("t_c", "FUTURE", "days", c(0, 1), 4L))
  cs <- CoordinateSystem$new("cs", axes)
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"),
                                  x_bounds = lon_b, y_bounds = lat_b, bounds_dims = c("time", "rlon", "rlat")),
               "`bounds_dims`")
})

test_that("set_geolocation() checks the shape of the boundary data", {
  cs <- wg_cs()
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"),
                                  x_bounds = lon_b[1:3, , ], y_bounds = lat_b[1:3, , ], bounds_dims = b_dims),
               "with 4 vertices")
  expect_error(cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"),
                                  x_bounds = aperm(lon_b), y_bounds = lat_b, bounds_dims = b_dims),
               "with 4 vertices")
})

test_that("set_geolocation() checks bounds_names", {
  cs <- wg_cs()
  expect_error(with_bounds(cs, bounds_names = c("lon", "lat_bnds")), "`bounds_names`")
  expect_error(with_bounds(cs, bounds_names = "lon_bnds"), "`bounds_names`")
})

test_that("set_convention() references boundary data", {
  g <- geo_of(set_convention(list(shape = c(4L, 3L)), with_bounds(wg_cs()), geolocation_group = "../geo"))$geodetic
  expect_identical(c(g$x$boundaries$ref$node, g$y$boundaries$ref$node), c("../geo/lon_bnds", "../geo/lat_bnds"))
})

test_that("write_geolocation() writes the boundary arrays", {
  s <- wg_root()
  wg_write(s$root, cs = with_bounds(wg_cs()))
  arr <- s$root$children$lat_bnds
  expect_identical(unlist(arr$metadata$dimension_names), b_dims)
  expect_identical(arr$attribute("units"), "degrees_north")
  expect_equal(arr[], lat_b)
})

test_that("geolocation() returns the boundary arrays", {
  s <- wg_root()
  g <- wg_write(s$root, cs = with_bounds(wg_cs()))$geolocation()
  expect_identical(names(g), c("x", "y", "x_bounds", "y_bounds"))
  expect_identical(g$x_bounds$name, "lon_bnds")
})

test_that("geolocation() returns the boundary arrays after reopening", {
  s <- wg_root()
  wg_write(s$root, cs = with_bounds(wg_cs()))
  g <- open_zarr(s$fn)[["/pr"]]$geolocation()
  expect_equal(g$y_bounds[], lat_b)
})

test_that("geolocation() without boundaries returns only x and y", {
  s <- wg_root()
  expect_identical(names(wg_write(s$root)$geolocation()), c("x", "y"))
})

test_that("arrays sharing geolocation verify the boundary arrays", {
  s <- wg_root()
  wg_write(s$root, "pr", cs = with_bounds(wg_cs()))
  expect_no_error(wg_write(s$root, "tas", cs = with_bounds(wg_cs())))
  cs <- wg_cs()
  cs$set_geolocation("geodetic", lon, lat, c("rlon", "rlat"), c("lon", "lat"), attributes = list(lon_atts, lat_atts),
                     x_bounds = lon_b + 1, y_bounds = lat_b, bounds_dims = b_dims)
  expect_error(wg_write(s$root, "huss", cs = cs), 'Incompatible external array "lon_bnds"')
})

# An array "chk" whose geolocation references existing lon/lat and the given
# boundary arrays, written beforehand with write_plain_array().
bounds_check <- function(lon_bnds_dims, lon_bnds_shape) {
  s <- wg_root()
  wg_write(s$root, cs = with_bounds(wg_cs()))
  write_plain_array(s$root, array(0, lon_bnds_shape), "bad_bnds", lon_bnds_dims)
  cs <- wg_cs()
  cs$geolocation <- zarr_convention_geolocation$new()$set("geodetic", "../lon", "../lat",
                                                          x_bounds = "../bad_bnds", y_bounds = "../lat_bnds")
  geozarr_array$new("chk", set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs), s$root, cs)
}

test_that("geolocation() accepts boundary arrays in any dimension order", {
  g <- bounds_check(c("rlat", "nv", "rlon"), c(3L, 4L, 4L))$geolocation()
  expect_identical(g$x_bounds$name, "bad_bnds")
})

test_that("geolocation() requires one vertex dimension in boundary arrays", {
  expect_error(bounds_check(c("rlon", "rlat"), c(4L, 3L))$geolocation(), "one vertex dimension")
  expect_error(bounds_check(c("nv", "rlon", "x"), c(4L, 4L, 3L))$geolocation(), "one vertex dimension")
})

test_that("geolocation() checks the lengths of boundary arrays", {
  expect_error(bounds_check(b_dims, c(3L, 4L, 3L))$geolocation(), "4 vertices")
  expect_error(bounds_check(c("nv", "rlat", "rlon"), c(4L, 4L, 3L))$geolocation(), "4 vertices")
})

test_that("geolocation() reports a missing boundary array", {
  s <- wg_root()
  wg_write(s$root, cs = with_bounds(wg_cs()))
  cs <- wg_cs()
  cs$geolocation <- zarr_convention_geolocation$new()$set("geodetic", "../lon", "../lat",
                                                          x_bounds = "../nope", y_bounds = "../lat_bnds")
  gza <- geozarr_array$new("chk", set_convention(zarr::define_array("float64", c(4L, 3L))$metadata(), cs), s$root, cs)
  expect_error(gza$geolocation(), 'Boundary array "../nope" not found')
})
