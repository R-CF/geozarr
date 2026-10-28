# geozarr_array: reading coordinate systems from stores, subset(), externals

# Fixtures ----------------------------------------------------------------------

# 5 x 6 x 4 array with predictable values
make_data <- function() array(seq_len(120L), c(5, 6, 4))

# Spatial: X regular, Y regular decreasing, categorical third axis
spatial_arr <- function() {
  a <- make_data()
  dimnames(a) <- list(x = 15 + 0:4 * 10, y = 75 - 0:5 * 10, cls = sprintf("b%d", 1:4))
  a
}

# cs: Y increasing, regular daily time axis
cs_arr <- function(time = sprintf("2026-01-%02d", 1:4)) {
  a <- make_data()
  dimnames(a) <- list(x = 15 + 0:4 * 10, y = 25 + 0:5 * 10, time = time)
  a
}

# Write a plain array carrying the given attributes into a new file store and
# return the path, so that the store can be re-opened through the domain.
store_with <- function(atts, dims = c("x", "y"), shape = c(4L, 3L), group_atts = NULL) {
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  root <- z[["/"]]
  if (!is.null(group_atts)) {
    m <- root$metadata
    m$attributes <- group_atts
    root$metadata <- m
    root$save()
  }
  write_plain_array(root, array(as.numeric(seq_len(prod(shape))), shape), "a", dims, atts)
  fn
}

spatial_atts <- function(...) {
  atts <- zarr_conv_spatial$new()$register(list())
  utils::modifyList(c(atts, list(`spatial:dimensions` = c("y", "x"),
                                 `spatial:transform` = c(10, 0, 15, 0, -10, 75))),
                    list(...))
}

cs_atts <- function(axes) {
  cs <- zarr_convention_cs$new()
  atts <- cs$register(list())
  cs$add_crs(axes)
  c(list(cs = list(crs = cs$crs)), atts)
}
csb <- zarr_convention_cs$new() # builder for axis definitions

# Construction ------------------------------------------------------------------

test_that("geozarr_array keeps a supplied coordinate system", {
  g <- as_geozarr(spatial_arr())
  expect_s3_class(g$coordinate_system, "CoordinateSystem")
  g$coordinate_system <- NULL
  expect_s3_class(g$coordinate_system, "CoordinateSystem")
  expect_identical(g$build_coordsys(), NULL) # already set: no-op
})

test_that("single-array store cannot hold external coordinates", {
  local_geozarr_option("max_explicit", 2L)
  expect_error(as_geozarr(cs_arr(sprintf("2026-%02d-01", 1:4))))
})

test_that("single-array store reports why external coordinates fail", {
  local_geozarr_option("max_explicit", 2L)
  expect_error(as_geozarr(cs_arr(sprintf("2026-%02d-01", 1:4))), "Single-array Zarr store")
})

# Reading "spatial" -------------------------------------------------------------

test_that("spatial store is re-opened with X, Y and ordinal axes", {
  fn <- tempfile(fileext = ".zarr")
  as_geozarr(spatial_arr(), "s", location = fn)
  r <- open_zarr(fn)[["/s"]]
  expect_s3_class(r, "geozarr_array")
  axes <- r$coordinate_system$axes
  expect_identical(names(axes), c("x", "y", "cls"))
  expect_identical(vapply(axes, function(a) a$abbreviation, ""), c(x = "X", y = "Y", cls = ""))
  expect_equal(axes$x$coordinates$values, 15 + 0:4 * 10)
  expect_equal(axes$y$coordinates$values, 75 - 0:5 * 10)
  expect_s3_class(axes$cls$coordinates, "CoordinatesOrdinal")
  expect_identical(axes$cls$coordinates$values, 0:3)
  expect_identical(r[], unname(make_data()))
})

test_that("spatial pixel registration gives cells extending right and down", {
  fn <- tempfile(fileext = ".zarr")
  as_geozarr(spatial_arr(), "s", location = fn)
  axes <- open_zarr(fn)[["/s"]]$coordinate_system$axes
  expect_equal(axes$x$coordinates$bounds_raw, c(-5, 5))
  expect_equal(axes$y$coordinates$bounds_raw, c(-5, 5))
})

test_that("spatial node registration has bounds", {
  fn <- store_with(spatial_atts(`spatial:registration` = "node"))
  axes <- open_zarr(fn)[["/a"]]$coordinate_system$axes
  expect_equal(axes$x$coordinates$bounds_raw, c(-5, 5))
  expect_equal(axes$x$coordinates$values, c(15, 25, 35, 45))
  expect_equal(axes$y$coordinates$values, c(75, 65, 55))
})

test_that("spatial axes follow the array's dimension order", {
  a <- array(seq_len(30L), c(6, 5))
  dimnames(a) <- list(y = 75 - 0:5 * 10, x = 15 + 0:4 * 10)
  fn <- tempfile(fileext = ".zarr")
  as_geozarr(a, "s", location = fn)
  r <- open_zarr(fn)[["/s"]]
  expect_identical(names(r$coordinate_system$axes), c("y", "x"))
  expect_identical(r$subset(X = c(20, 40))$shape, c(6L, 2L))
})

test_that("spatial metadata errors are reported", {
  expect_error(open_zarr(store_with(spatial_atts(`spatial:dimensions` = "y"))),
               "wrong number of items")
  expect_error(open_zarr(store_with(spatial_atts(`spatial:dimensions` = c("lat", "x")))),
               "names not present")
  expect_error(open_zarr(store_with(spatial_atts(`spatial:transform_type` = "rpc"))),
               'Only "affine"')
  expect_error(open_zarr(store_with(spatial_atts(`spatial:transform` = 1:5))),
               '"spatial:transform" has wrong number')
  expect_error(open_zarr(store_with(spatial_atts(`spatial:bbox` = 1:3))),
               '"spatial:bbox" has wrong number')
  expect_error(open_zarr(store_with(spatial_atts(`spatial:shape` = 1L))),
               '"spatial:shape" has wrong number')
  expect_error(open_zarr(store_with(spatial_atts(`spatial:registration` = "corner"))),
               "bad value")
})

test_that("spatial metadata requires dimensions and transform", {
  atts <- spatial_atts()
  expect_error(open_zarr(store_with(atts[names(atts) != "spatial:dimensions"])),
               '"spatial:dimensions" not found')
  expect_error(open_zarr(store_with(atts[names(atts) != "spatial:transform"])),
               '"spatial:transform" not found')
})

test_that("spatial attributes may come from the parent group", {
  grp_atts <- spatial_atts()
  arr_atts <- zarr_conv_spatial$new()$register(list())
  fn <- store_with(arr_atts, group_atts = grp_atts)
  z <- open_zarr(fn)
  expect_s3_class(z[["/"]], "geozarr_group")
  expect_equal(z[["/a"]]$coordinate_system$axes$x$coordinates$values, c(20, 30, 40, 50))
})

# Reading "cs" ------------------------------------------------------------------

test_that("cs store is re-opened with regular, explicit and time axes", {
  fn <- tempfile(fileext = ".zarr")
  as_geozarr(cs_arr(), "v", location = fn)
  r <- open_zarr(fn)[["/v"]]
  axes <- r$coordinate_system$axes
  expect_identical(names(axes), c("x", "y", "time"))
  expect_s3_class(axes$x$coordinates, "CoordinatesPacked")
  expect_s3_class(axes$time$coordinates, "CoordinatesTime")
  expect_identical(axes$time$coordinates$values, sprintf("2026-01-%02d", 1:4))
  expect_identical(axes$time$abbreviation, "T")
  expect_identical(axes$time$coordinates$direction, "FUTURE")
})

test_that("cs store with external coordinates and bounds is re-opened", {
  local_geozarr_option("max_explicit", 2L)
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  g <- z$add_group("/", "g")
  create_geozarr_array("a", g, list(x = list(coordinates = c(1, 2, 4), boundaries = rbind(c(.5, 1.5, 3), c(1.5, 3, 5))),
                                    y = list(coordinates = c(5, 6, 8))), "float64")
  axes <- open_zarr(fn)[["/g/a"]]$coordinate_system$axes
  expect_identical(axes$x$coordinates$values, c(1, 2, 4))
  expect_identical(axes$x$coordinates$bounds, rbind(c(.5, 1.5, 3), c(1.5, 3, 5)))
  expect_identical(axes$y$coordinates$values, c(5, 6, 8))
})

test_that("cs explicit regular values are packed on reading", {
  axes <- list(x = csb$axis(list(csb$coordinates(csb$values_explicit(c(15, 25, 35, 45)), unit = "m")), "X", "EAST"),
               y = csb$axis(list(csb$coordinates(csb$values_explicit(c(5, 6, 8)))), "Y", "NORTH"))
  a <- open_zarr(store_with(cs_atts(axes)))[["/a"]]
  expect_s3_class(a$coordinate_system$axes$x$coordinates, "CoordinatesPacked")
  expect_identical(a$coordinate_system$axes$x$coordinates$unit, "m")
})

test_that("cs explicit irregular values survive a round trip", {
  axes <- list(x = csb$axis(list(csb$coordinates(csb$values_regular(15, 10))), "X"),
               y = csb$axis(list(csb$coordinates(csb$values_explicit(c(5, 6, 8)))), "Y"))
  a <- open_zarr(store_with(cs_atts(axes)))[["/a"]]
  expect_equal(a$coordinate_system$axes$y$coordinates$values, c(5, 6, 8))
})

test_that("cs axis with several coordinate sets", {
  x <- csb$axis(list(csb$coordinates(csb$values_regular(15, 10), name = "easting"),
                     csb$coordinates(csb$values_explicit(c("a", "b", "c", "d")))), "X")
  y <- csb$axis(list(csb$coordinates(csb$values_regular(5, 1))), "Y")
  a <- open_zarr(store_with(cs_atts(list(x = x, y = y))))[["/a"]]
  expect_identical(names(a$coordinate_system$axes$x$coordinates_list), c("easting", "x_coordinates2"))
  expect_identical(a$coordinate_system$axes$x$coordinates$name, "easting")
})

test_that("cs unknown direction falls back to OTHER", {
  x <- csb$axis(list(csb$coordinates(csb$values_regular(15, 10))), "X")
  x$direction <- "sideways"
  y <- csb$axis(list(csb$coordinates(csb$values_regular(5, 1))), "Y")
  a <- open_zarr(store_with(cs_atts(list(x = x, y = y))))[["/a"]]
  expect_identical(a$coordinate_system$axes$x$coordinates$direction, "OTHER")
})

test_that("cs scalar axes are appended after the dimensional axes", {
  x <- csb$axis(list(csb$coordinates(csb$values_regular(15, 10))), "X")
  y <- csb$axis(list(csb$coordinates(csb$values_regular(5, 1))), "Y")
  h <- csb$axis(list(csb$coordinates(csb$values_explicit(2), unit = "m")), "Z", "UP")
  a <- open_zarr(store_with(cs_atts(list(x = x, y = y, height = h))))[["/a"]]
  axes <- a$coordinate_system$axes
  expect_identical(names(axes), c("x", "y", "height"))
  expect_identical(axes$height$length, 1L)
  expect_identical(axes$height$coordinates$direction, "OTHER")
})

test_that("cs dimension without an axis definition gets an ordinal axis", {
  x <- csb$axis(list(csb$coordinates(csb$values_regular(15, 10))), "X")
  a <- open_zarr(store_with(cs_atts(list(x = x))))[["/a"]]
  expect_identical(a$coordinate_system$axes$y$coordinates$values, 0:2)
})

test_that("cs metadata errors are reported", {
  cmo <- zarr_convention_cs$new()$register(list())
  expect_error(open_zarr(store_with(cmo)), 'Required attribute "cs"')
  expect_error(open_zarr(store_with(c(cmo, list(cs = list(crs = list()))))), "at least one CRS")
  x_ok <- csb$axis(list(csb$coordinates(csb$values_regular(15, 10))), "X")
  bad <- function(coord) cs_atts(list(x = x_ok, y = list(abbreviation = "Y", coordinates = list(coord))))
  expect_error(open_zarr(store_with(bad(list(unit = "m")))), 'missing required "values"')
  expect_error(open_zarr(store_with(bad(list(values = list(regular = c(1, 2, 3)))))),
               'must have exactly 2 elements')
  expect_error(open_zarr(store_with(bad(list(values = list(other = 1))))),
               'one of "regular", "explicit", or "external"')
  expect_error(open_zarr(store_with(bad(list(values = list(regular = c(5, 1)), boundaries = list(regular = 1))))),
               "boundaries must have exactly 2 elements")
  expect_error(open_zarr(store_with(bad(list(values = list(regular = c(5, 1)), boundaries = list(x = 1))))),
               'boundaries must have one of "regular" or "external"')
  expect_error(open_zarr(store_with(bad(list(values = list(external = list(ref = list())))))),
               'missing the required "node"')
  expect_error(open_zarr(store_with(bad(list(values = list(external = list(ref = list(node = "../y", attribute = "/a"))))))),
               "attribute reference which is not supported")
  expect_error(open_zarr(store_with(bad(list(values = list(external = list(ref = list(node = "y"))))))),
               "relative path")
  expect_error(open_zarr(store_with(bad(list(values = list(external = list(ref = list(node = "/y", uri = "s3://b/s.zarr"))))))),
               "Not yet supported")
})

test_that("cs external node that does not exist warns", {
  x_ok <- csb$axis(list(csb$coordinates(csb$values_regular(15, 10))), "X")
  y <- csb$axis(list(csb$coordinates(csb$values_external("../nope"))), "Y")
  expect_warning(try(open_zarr(store_with(cs_atts(list(x = x_ok, y = y)))), silent = TRUE),
                 'External node "../nope" not found')
})

# Reading XArray ----------------------------------------------------------------

xarray_store <- function() {
  fn <- tempfile(fileext = ".zarr")
  root <- create_zarr(fn)[["/"]]
  write_plain_array(root, c(15, 25, 35, 45), "lon", "lon", list(units = "degrees_east"))
  write_plain_array(root, c(5, 6, 8), "lat", "lat", list(units = "degrees_north"))
  write_plain_array(root, array(as.numeric(1:12), c(4, 3)), "tas", c("lon", "lat"), list(coordinates = "lon lat"))
  write_plain_array(root, array(as.numeric(1:8), c(4, 2)), "ens", c("lon", "member"), list(coordinates = "lon"))
  write_plain_array(root, array(as.numeric(1:8), c(4, 2)), "bare", c("lon", "member"))
  fn
}

test_that("XArray data variables become geozarr arrays", {
  z <- open_zarr(xarray_store())
  expect_s3_class(z[["/tas"]], "geozarr_array")
  expect_s3_class(z[["/ens"]], "geozarr_array")
  expect_false(inherits(z[["/lon"]], "geozarr_array"))  # dimension coordinate
  expect_false(inherits(z[["/bare"]], "geozarr_array")) # no "coordinates" attribute
})

test_that("XArray coordinate system from dimension coordinates", {
  axes <- open_zarr(xarray_store())[["/tas"]]$coordinate_system$axes
  expect_identical(vapply(axes, function(a) a$abbreviation, ""), c(lon = "X", lat = "Y"))
  expect_s3_class(axes[[1L]]$coordinates, "CoordinatesPacked")
  expect_identical(axes[[1L]]$coordinates$unit, "degrees_east")
  expect_identical(axes[[2L]]$coordinates$values, c(5, 6, 8))
})

test_that("XArray dimension without coordinates gets an ordinal axis", {
  axes <- open_zarr(xarray_store())[["/ens"]]$coordinate_system$axes
  expect_s3_class(axes[[2L]]$coordinates, "CoordinatesOrdinal")
  expect_identical(axes[[2L]]$coordinates$values, 0:1)
})

test_that("XArray arrays can be subset", {
  a <- open_zarr(xarray_store())[["/tas"]]
  expect_identical(a$subset(X = c(20, 40))$shape, c(2L, 3L))
  expect_identical(a$subset(lat = c(5.5, 9))$shape, c(4L, 2L))
})

test_that("XArray coordinate system has named axes", {
  expect_identical(names(open_zarr(xarray_store())[["/tas"]]$coordinate_system$axes), c("lon", "lat"))
})

# subset() ----------------------------------------------------------------------

test_that("subset() by abbreviation and by name", {
  g <- as_geozarr(spatial_arr())
  s <- g$subset(X = c(20, 40))
  expect_identical(s$shape, c(2L, 6L, 4L))
  expect_identical(s$coordinate_system$axes$x$coordinates$values, c(25, 35))
  expect_identical(s[], unname(make_data()[2:3, , ]))
  expect_identical(g$subset(x = c(20, 40), y = c(60, 40))$shape, c(2L, 2L, 4L))
  expect_identical(g$subset(y = c(60, 40), X = c(20, 40))$shape, c(2L, 2L, 4L))
})

test_that("subset() accepts a list of selectors", {
  g <- as_geozarr(spatial_arr())
  expect_identical(g$subset(list(x = c(20, 40)))$shape, c(2L, 6L, 4L))
})

test_that("subset() on string coordinates", {
  g <- as_geozarr(spatial_arr())
  s <- g$subset(cls = c("b2", "b3"))
  expect_identical(s$shape, c(5L, 6L, 2L))
  expect_identical(s$coordinate_system$axes$cls$coordinates$values, c("b2", "b3"))
})

test_that("subset() outside the axis returns NULL", {
  expect_null(as_geozarr(spatial_arr())$subset(X = c(1000, 2000)))
})

test_that("subset() rejects duplicate selectors", {
  g <- as_geozarr(spatial_arr())
  expect_error(g$subset(x = c(20, 40), x = c(20, 30)), "Duplicated axis names")
  expect_error(g$subset(X = c(20, 40), X = c(20, 30)), "Duplicated axis abbreviations")
})

test_that("subset() rejects a name and abbreviation for the same axis", {
  g <- as_geozarr(spatial_arr())
  expect_error(g$subset(x = c(20, 40), X = c(20, 30)), "Duplicated")
})

test_that("subset() on time coordinates keeps the time axis", {
  g <- as_geozarr(cs_arr(), "v")
  s <- g$subset(T = c("2026-01-02", "2026-01-04"))
  expect_identical(s$shape, c(5L, 6L, 2L))
  expect_identical(s$coordinate_system$axes$time$coordinates$values, c("2026-01-02", "2026-01-03"))
  s <- g$subset(time = "2026-01-02")
  expect_identical(s$shape, c(5L, 6L, 1L))
  expect_identical(as.vector(s[]), as.vector(make_data()[, , 2]))
})

test_that("subset() records history and keeps other attributes", {
  g <- as_geozarr(cs_arr(), "v")
  g$set_attribute("title", "test")
  s <- g$subset(X = c(20, 40))
  expect_identical(s$attributes$title, "test")
  expect_match(s$attributes$history, "Subsetting array with R package geozarr")
  expect_null(s$attributes$spatial)
})

test_that("subset() into a group of an existing store", {
  g <- as_geozarr(cs_arr(), "v")
  z <- create_zarr()
  grp <- z$add_group("/", "out")
  s <- g$subset(Y = c(30, 50), .name = "sub", .location = grp)
  expect_identical(z$arrays, "/out/sub")
  expect_identical(s$shape, c(5L, 2L, 4L))
  expect_error(g$subset(Y = c(30, 50), .name = "sub", .location = grp), "already has a node")
})

test_that("subset() into a new file store", {
  g <- as_geozarr(cs_arr(), "v")
  fn <- tempfile(fileext = ".zarr")
  g$subset(X = c(20, 40), .name = "sub", .location = fn)
  r <- open_zarr(fn)[["/sub"]]
  expect_s3_class(r, "geozarr_array")
  expect_identical(r$shape, c(2L, 6L, 4L))
})

test_that("subset() of a re-opened cs array with a time axis", {
  fn <- tempfile(fileext = ".zarr")
  as_geozarr(cs_arr(), "v", location = fn)
  s <- open_zarr(fn)[["/v"]]$subset(T = c("2026-01-01", "2026-01-03"))
  expect_identical(s$shape, c(5L, 6L, 2L))
})

test_that("subset() of a re-opened spatial array with a categorical axis", {
  fn <- tempfile(fileext = ".zarr")
  as_geozarr(spatial_arr(), "s", location = fn)
  # Re-opened pixel-registered cells have bounds, so [20, 40] overlaps 3 cells
  s <- open_zarr(fn)[["/s"]]$subset(X = c(20, 40))
  expect_identical(s$shape, c(2L, 6L, 4L))
})
