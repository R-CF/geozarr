# CRS objects defined in a group and referenced from arrays (cs convention)

shared_crs <- function() {
  csb <- zarr_convention_cs$new()
  list(
    WGS84 = list(
      type = "planar",
      axes = list(
        lon = csb$axis(list(csb$coordinates(csb$values_regular(-10, 1))), "X", "EAST"),
        lat = csb$axis(list(csb$coordinates(csb$values_regular(40, 1))), "Y", "NORTH")),
      id = list(`proj:code` = "EPSG:4326"),
      geolocation = list(geodetic = list(x = list(values = list(ref = list(node = "glon"))),
                                         y = list(values = list(ref = list(node = "glat")))))),
    cal = list(
      type = "temporal",
      axes = list(time = csb$axis(list(csb$coordinates(csb$values_external("time"),
                                                      time = csb$time("days", "2000-01-01"))), "T")))
  )
}

crs_ref <- function(node, key) list(ref = list(node = node, attribute = paste0("/attributes/crs/", key)))

array_atts <- function(node, keys = c("WGS84", "cal")) {
  c(zarr::zarr_convention_ref$new()$register(zarr_convention_cs$new()$register(list())),
    list(cs = list(crs = lapply(keys, function(k) crs_ref(node, k)))))
}

# Root group with the shared CRS objects, the "time", "glon" and "glat"
# arrays, array "a" and array "/sub/b". `group_geo` declares the geolocation
# convention on the root group, `crs` replaces the shared CRS objects.
shared_store <- function(group_geo = TRUE, crs = shared_crs(), a_atts = array_atts("..")) {
  fn <- tempfile(fileext = ".zarr")
  root <- create_zarr(fn)[["/"]]
  convs <- c(list(zarr_convention_cs$new(), zarr::zarr_convention_ref$new(), zarr_conv_proj$new()),
             if (group_geo) list(zarr_convention_geolocation$new()))
  m <- root$metadata
  m$attributes <- c(Reduce(function(atts, cv) cv$register(atts), convs, list()), list(crs = crs))
  root$metadata <- m
  root$save()

  write_plain_array(root, c(0, 31, 59), "time", "time")
  write_plain_array(root, array(as.numeric(1:8), c(4L, 2L)), "glon", c("lon", "lat"))
  write_plain_array(root, array(as.numeric(1:8), c(4L, 2L)), "glat", c("lon", "lat"))
  write_plain_array(root, array(as.numeric(1:24), c(4L, 2L, 3L)), "a", c("lon", "lat", "time"), a_atts)
  sub <- root$add_group("sub")
  write_plain_array(sub, array(as.numeric(1:24), c(4L, 2L, 3L)), "b", c("lon", "lat", "time"), array_atts("../.."))
  fn
}

coord_values <- function(gza, axis) gza$coordinate_system$axes[[axis]]$coordinates$values

# Resolving shared CRS objects ---------------------------------------------------

test_that("array resolves the axes of a shared CRS", {
  a <- open_zarr(shared_store())[["/a"]]
  abbr <- vapply(a$coordinate_system$axes, function(ax) ax$abbreviation, character(1L))
  expect_identical(unname(abbr), c("X", "Y", "T"))
  expect_equal(coord_values(a, "lon"), c(-10, -9, -8, -7))
})

test_that("external values in a shared CRS resolve from the group", {
  a <- open_zarr(shared_store())[["/a"]]
  expect_equal(a$coordinate_system$axes$time$coordinates$offsets, c(0, 31, 59))
})

test_that("array in a subgroup resolves the same shared CRS", {
  z <- open_zarr(shared_store())
  expect_equal(coord_values(z[["/sub/b"]], "lat"), coord_values(z[["/a"]], "lat"))
  expect_equal(z[["/sub/b"]]$coordinate_system$axes$time$coordinates$offsets, c(0, 31, 59))
})

test_that("geolocation in a shared CRS resolves from the group", {
  z <- open_zarr(shared_store())
  expect_identical(z[["/a"]]$geolocation()$x$name, "glon")
  expect_identical(z[["/sub/b"]]$geolocation()$y$name, "glat")
})

test_that("geolocation in a shared CRS requires the convention on the group", {
  a_atts <- zarr_convention_geolocation$new()$register(array_atts(".."))
  a <- open_zarr(shared_store(group_geo = FALSE, a_atts = a_atts))[["/a"]]
  expect_null(a$coordinate_system$geolocation)
})

test_that("subset() keeps the id of a shared CRS", {
  a <- open_zarr(shared_store())[["/a"]]
  crs <- a$subset(X = c(-10, -9))$metadata$attributes$cs$crs
  planar <- crs[[which(vapply(crs, function(c) c$type, character(1L)) == "planar")]]
  expect_identical(planar$id, list(`proj:code` = "EPSG:4326"))
})

# Errors ---------------------------------------------------------------------------

test_that("a CRS pointer that does not resolve is an error", {
  expect_error(open_zarr(shared_store(a_atts = array_atts("..", "nope"))),
               'attribute "/attributes/crs/nope" does not resolve to a CRS object')
})

test_that("a CRS reference without a pointer is an error", {
  atts <- array_atts("..")
  atts$cs$crs[[1L]]$ref$attribute <- NULL
  expect_error(open_zarr(shared_store(a_atts = atts)), "does not resolve to a CRS object")
})

test_that("a CRS reference to another store is an error", {
  atts <- array_atts("..")
  atts$cs$crs[[1L]]$ref <- list(uri = "s3://bucket/x.zarr", node = "/", attribute = "/attributes/crs/WGS84")
  expect_error(open_zarr(shared_store(a_atts = atts)), "external store are not yet supported")
})

# Helpers ----------------------------------------------------------------------------

test_that(".json_pointer() resolves object members and array elements", {
  doc <- list(attributes = list(crs = list(WGS84 = 1, list_of = list("a", "b"))))
  expect_identical(.json_pointer(doc, "/attributes/crs/WGS84"), 1)
  expect_identical(.json_pointer(doc, "/attributes/crs/list_of/1"), "b")
  expect_identical(.json_pointer(doc, ""), doc)
})

test_that(".json_pointer() unescapes ~1 and ~0", {
  doc <- list(`a/b` = list(`c~d` = 2))
  expect_identical(.json_pointer(doc, "/a~1b/c~0d"), 2)
})

test_that(".json_pointer() returns NULL when the pointer does not resolve", {
  doc <- list(a = list("x"), b = 1)
  expect_null(.json_pointer(doc, "/nope"))
  expect_null(.json_pointer(doc, "/a/1"))
  expect_null(.json_pointer(doc, "/a/01"))
  expect_null(.json_pointer(doc, "/b/c"))
})

test_that(".rebase_refs() prefixes relative references at any depth", {
  x <- list(values = list(external = list(ref = list(node = "time"))),
            geolocation = list(geodetic = list(x = list(values = list(ref = list(node = "../lon"))))))
  y <- .rebase_refs(x, "../..")
  expect_identical(y$values$external$ref$node, "../../time")
  expect_identical(y$geolocation$geodetic$x$values$ref$node, "../../../lon")
})

test_that(".rebase_refs() leaves references to another store unchanged", {
  x <- list(external = list(ref = list(uri = "s3://b/x.zarr", node = "/time")))
  expect_identical(.rebase_refs(x, ".."), x)
})

test_that(".rebase_refs() keeps NULL elements", {
  x <- list(a = NULL, ref = list(node = "t"))
  expect_identical(names(.rebase_refs(x, "..")), c("a", "ref"))
})
