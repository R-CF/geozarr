# zarr_conv_proj

test_that("proj convention identity", {
  p <- zarr_conv_proj$new()
  expect_s3_class(p, "zarr_convention")
  expect_identical(p$name, "proj")
  expect_identical(p$uuid, "f17cb550-5864-4468-aeb7-f3180cfb622f")
  expect_match(p$schema, "/proj/refs/tags/v0.1/schema.json$")
  expect_match(p$spec, "/proj/blob/v0.1/README.md$")
})

test_that("proj convention description", {
  expect_identical(zarr_conv_proj$new()$description,
                   "Coordinate reference system information for geospatial data")
})

test_that("proj convention registers its CMO once", {
  p <- zarr_conv_proj$new()
  atts <- p$register(list(a = 1))
  expect_length(atts$zarr_conventions, 1L)
  expect_identical(atts$zarr_conventions[[1L]]$uuid, p$uuid)
  expect_identical(p$register(atts), atts)
})

test_that("proj convention attributes from the constructor", {
  p <- zarr_conv_proj$new(list(code = "EPSG:4326", wkt2 = "GEOGCRS[\"WGS 84\"]", projjson = "{\"type\":\"x\"}"))
  expect_identical(p$code, "EPSG:4326")
  expect_identical(p$wkt2, "GEOGCRS[\"WGS 84\"]")
  expect_identical(p$projjson, "{\"type\":\"x\"}")
  expect_identical(names(p$as_list()), c("proj:code", "proj:wkt2", "proj:projjson"))
})

test_that("proj convention constructor rejects unknown attributes", {
  expect_error(zarr_conv_proj$new(list(epsg = 4326)), "misformed")
  expect_error(zarr_conv_proj$new("EPSG:4326"), "misformed")
})

test_that("proj convention constructor accepts any WKT2 text", {
  wkt <- "GEOGCRS[\"WGS 84\", REMARK[\"Earth's ellipsoid\"]]"
  expect_identical(zarr_conv_proj$new(list(wkt2 = wkt))$wkt2, wkt)
})

test_that("proj convention as_list() requires an attribute", {
  expect_error(zarr_conv_proj$new()$as_list(), "At least one of the attributes")
})

test_that("proj convention as_list() includes only set attributes", {
  p <- zarr_conv_proj$new()
  p$wkt2 <- "GEOGCRS[]"
  expect_identical(p$as_list(), list(`proj:wkt2` = "GEOGCRS[]"))
})

test_that("proj convention setters validate", {
  p <- zarr_conv_proj$new()
  p$code <- "EPSG:3857"
  expect_identical(p$code, "EPSG:3857")
  expect_error(p$code <- character(0), "right format")
  expect_error(p$code <- "4326", "right format")
  expect_error(p$wkt2 <- 1, "WKT2")
  expect_error(p$projjson <- c("a", "b"), "PROJJSON")
})

test_that("proj code must be exactly authority:code", {
  p <- zarr_conv_proj$new()
  expect_error(p$code <- "see EPSG:4326 please", "right format")
})

test_that("proj code accepts non-numeric authority codes", {
  p <- zarr_conv_proj$new()
  p$code <- "OGC:CRS84"
  expect_identical(p$code, "OGC:CRS84")
})
