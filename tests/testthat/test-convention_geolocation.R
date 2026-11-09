# zarr_convention_geolocation

test_that("geolocation convention identity", {
  g <- zarr_convention_geolocation$new()
  expect_identical(g$name, "geolocation")
  expect_identical(g$uuid, "bb9ee930-8c60-4c47-ad6b-8daa558987ed")
  expect_match(g$schema, "zarr_convention_geolocation/refs/tags/v2.0.0/schema.json$")
  expect_match(g$spec, "zarr_convention_geolocation/blob/v2.0.0/README.md$")
  expect_identical(g$description, "Convention for storing geolocation arrays")
})

test_that("geolocation register() also registers ref", {
  atts <- zarr_convention_geolocation$new()$register(list())
  uuids <- vapply(atts$zarr_conventions, function(c) c$uuid, character(1L))
  expect_identical(uuids, c("bb9ee930-8c60-4c47-ad6b-8daa558987ed",
                            "d89b30cf-ed8c-43d5-9a16-b492f0cd8786"))
})

test_that("geolocation register() does not duplicate an existing ref entry", {
  atts <- zarr::zarr_convention_ref$new()$register(list())
  atts <- zarr_convention_geolocation$new()$register(atts)
  expect_length(atts$zarr_conventions, 2L)
})

test_that("geolocation as_list() requires arrays to be set", {
  expect_error(zarr_convention_geolocation$new()$as_list(), "`set\\(\\)`")
})

test_that("geolocation set() geodetic in-store", {
  g <- zarr_convention_geolocation$new()
  expect_invisible(g$set("geodetic", "../longitude", "../latitude"))
  expect_identical(g$as_list(),
                   list(geodetic = list(x = list(values = list(ref = list(node = "../longitude"))),
                                        y = list(values = list(ref = list(node = "../latitude"))))))
})

test_that("geolocation set() with uri and id", {
  g <- zarr_convention_geolocation$new()
  g$set("planar", "/utm/easting", "/utm/northing", uri = "s3://bucket/geo.zarr",
        id = zarr_conv_proj$new(list(code = "EPSG:25828")))
  pl <- g$as_list()$planar
  expect_identical(pl$x, list(values = list(ref = list(uri = "s3://bucket/geo.zarr", node = "/utm/easting"))))
  expect_identical(pl$y$values$ref$node, "/utm/northing")
  expect_identical(pl$id, list(`proj:code` = "EPSG:25828"))
})

test_that("geolocation as_list() orders geodetic before planar", {
  g <- zarr_convention_geolocation$new()
  g$set("planar", "../e", "../n")
  g$set("geodetic", "../lon", "../lat")
  expect_identical(names(g$as_list()), c("geodetic", "planar"))
})

test_that("geolocation set() replaces an earlier setting of the same kind", {
  g <- zarr_convention_geolocation$new()
  g$set("geodetic", "../lon", "../lat")
  g$set("geodetic", "../lon2", "../lat2")
  expect_identical(g$as_list()$geodetic$x$values$ref$node, "../lon2")
})

test_that("geolocation set() validates arguments", {
  g <- zarr_convention_geolocation$new()
  expect_error(g$set("polar", "../x", "../y"), "should be one of")
  expect_error(g$set("geodetic", "", "../lat"), "`node`")
  expect_error(g$set("geodetic", "../lon", "../lat", uri = ""), "`uri`")
  expect_error(g$set("geodetic", "../lon", "../lat", id = "EPSG:4326"), "zarr_conv_proj")
})

test_that("geolocation clear()", {
  g <- zarr_convention_geolocation$new()
  g$set("geodetic", "../lon", "../lat")
  g$clear()
  expect_error(g$as_list(), "`set\\(\\)`")
})

test_that("geolocation initialize() takes a geolocation object", {
  geo <- list(planar = list(x = list(values = list(ref = list(uri = "s3://a", node = "/e"))),
                            y = list(values = list(ref = list(uri = "s3://b", node = "/n")))))
  expect_identical(zarr_convention_geolocation$new(geo)$as_list(), geo)
})

test_that("geolocation initialize() validates the geolocation object", {
  ref <- list(values = list(ref = list(node = "../lon")))
  expect_error(zarr_convention_geolocation$new(list()), "misformed")
  expect_error(zarr_convention_geolocation$new(list(list(x = ref, y = ref))), "misformed")
  expect_error(zarr_convention_geolocation$new(list(polar = list(x = ref, y = ref))), "misformed")
  expect_error(zarr_convention_geolocation$new(list(geodetic = list(x = ref))), "misformed")
  expect_error(zarr_convention_geolocation$new(list(geodetic = list(x = ref, y = list(values = list(ref = list()))))), "misformed")
  expect_error(zarr_convention_geolocation$new(list(geodetic = list(x = ref, y = ref),
                                                    geodetic = list(x = ref, y = ref))), "misformed")
})

test_that("geolocation register() adds proj only when an id is present", {
  g <- zarr_convention_geolocation$new()
  g$set("geodetic", "../lon", "../lat")
  expect_length(g$register(list())$zarr_conventions, 2L)
  g$set("planar", "../e", "../n", id = zarr_conv_proj$new(list(code = "EPSG:25828")))
  names <- vapply(g$register(list())$zarr_conventions, function(c) c$name, character(1L))
  expect_identical(names, c("geolocation", "ref", "proj"))
})

test_that("geolocation initialize() rejects the v1 structure", {
  v1 <- list(geodetic = list(x = list(ref = list(node = "../lon")), y = list(ref = list(node = "../lat"))))
  expect_error(zarr_convention_geolocation$new(v1), "misformed")
})

test_that("geolocation initialize() takes boundaries for both x and y", {
  crd <- function(n) list(values = list(ref = list(node = n)), boundaries = list(ref = list(node = paste0(n, "_bnds"))))
  geo <- list(geodetic = list(x = crd("../lon"), y = crd("../lat")))
  expect_identical(zarr_convention_geolocation$new(geo)$as_list(), geo)
})

test_that("geolocation initialize() rejects boundaries for only one of x and y", {
  geo <- list(geodetic = list(x = list(values = list(ref = list(node = "../lon")),
                                       boundaries = list(ref = list(node = "../lon_bnds"))),
                              y = list(values = list(ref = list(node = "../lat")))))
  expect_error(zarr_convention_geolocation$new(geo), "misformed")
})

test_that("geolocation initialize() rejects malformed boundaries", {
  geo <- list(geodetic = list(x = list(values = list(ref = list(node = "../lon")), boundaries = list(node = "../lon_bnds")),
                              y = list(values = list(ref = list(node = "../lat")), boundaries = list(node = "../lat_bnds"))))
  expect_error(zarr_convention_geolocation$new(geo), "misformed")
})

test_that("geolocation set() with boundaries", {
  g <- zarr_convention_geolocation$new()
  g$set("geodetic", "../lon", "../lat", x_bounds = "../lon_bnds", y_bounds = "../lat_bnds")
  geo <- g$as_list()$geodetic
  expect_identical(geo$x$boundaries, list(ref = list(node = "../lon_bnds")))
  expect_identical(geo$y$values, list(ref = list(node = "../lat")))
})

test_that("geolocation set() requires boundaries for both x and y", {
  g <- zarr_convention_geolocation$new()
  expect_error(g$set("geodetic", "../lon", "../lat", x_bounds = "../lon_bnds"), "both be given, or neither")
})
