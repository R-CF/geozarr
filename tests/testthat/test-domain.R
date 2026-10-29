# zarr_domain_geozarr and geozarr_group

test_that("GeoZarr domain is registered with zarr", {
  d <- zarr_domain_geozarr$new()
  expect_s3_class(d, "zarr_domain")
  expect_identical(d$name, "GeoZarr")
})

test_that("domain declines nodes without GeoZarr conventions", {
  d <- zarr_domain_geozarr$new()
  expect_false(d$build("g", list(node_type = "group", attributes = list()), NULL))
  expect_false(d$build("a", list(node_type = "array", attributes = list(), dimension_names = "x"), NULL))
})

# Through real stores -----------------------------------------------------------

group_store <- function(group_atts, array_atts) {
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  root <- z[["/"]]
  m <- root$metadata
  m$attributes <- group_atts
  root$metadata <- m
  root$save()
  write_plain_array(root, array(as.numeric(1:12), c(4, 3)), "a", c("x", "y"), array_atts)
  fn
}

spatial_group_atts <- function() {
  c(zarr_conv_spatial$new()$register(list(title = "grp")),
    list(`spatial:dimensions` = c("y", "x"), `spatial:transform` = c(10, 0, 15, 0, -10, 75)))
}

test_that("group with a GeoZarr convention becomes a geozarr_group", {
  z <- open_zarr(group_store(spatial_group_atts(), zarr_conv_spatial$new()$register(list())))
  expect_s3_class(z[["/"]], "geozarr_group")
  expect_s3_class(z[["/"]], "zarr_group")
})

test_that("geozarr_group hides convention attributes when printing", {
  local_plain_theme()
  z <- open_zarr(group_store(spatial_group_atts(), zarr_conv_spatial$new()$register(list())))
  out <- capture.output(print(z[["/"]]))
  expect_true(any(grepl("title", out)))
  expect_false(any(grepl("spatial:", out, fixed = TRUE)))
  expect_false(any(grepl("zarr_conventions", out, fixed = TRUE)))
})

test_that("plain arrays stay plain zarr arrays", {
  z <- open_zarr(group_store(list(), list()))
  expect_false(inherits(z[["/a"]], "geozarr_array"))
  expect_false(inherits(z[["/"]], "geozarr_group"))
})

test_that("array with only a non-GeoZarr convention stays a plain array", {
  uom <- zarr::zarr_convention_uom$new()$register(list(units = "K"))
  z <- open_zarr(group_store(list(), uom))
  expect_false(inherits(z[["/a"]], "geozarr_array"))
})

test_that("array with another convention under a spatial group is claimed", {
  uom <- zarr::zarr_convention_uom$new()$register(list())
  z <- open_zarr(group_store(spatial_group_atts(), uom))
  expect_s3_class(z[["/a"]], "geozarr_array")
})

test_that("array listing an unsupported convention among GeoZarr ones fails to open", {
  atts <- zarr_conv_spatial$new()$register(list(`spatial:dimensions` = c("y", "x"),
                                                `spatial:transform` = c(10, 0, 15, 0, -10, 75)))
  atts$zarr_conventions <- c(atts$zarr_conventions, list(list(name = "bogus", uuid = "0000")))
  expect_error(open_zarr(group_store(list(), atts)), "unsupported conventions: bogus")
})

test_that("array with a brief CMO (schema only, no uuid) is claimed", {
  atts <- zarr_conv_spatial$new()$register(list(`spatial:dimensions` = c("y", "x"),
                                                `spatial:transform` = c(10, 0, 15, 0, -10, 75)),
                                           brief = TRUE)
  expect_null(atts$zarr_conventions[[1L]]$uuid)
  z <- open_zarr(group_store(list(), atts))
  expect_s3_class(z[["/a"]], "geozarr_array")
})
