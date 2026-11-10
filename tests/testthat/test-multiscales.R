# Reading multiscales data sets

# A two-level pyramid, 4 x 4 cells of 10 m and 2 x 2 cells of 20 m. With
# `group_layout` the assets are groups holding array "data" and a coordinate
# array "x", otherwise the assets are the arrays themselves.
multiscales_store <- function(group_layout = TRUE, group_atts = list()) {
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  root <- z[["/"]]

  ms <- zarr_convention_multiscales$new()
  ms$add_level("0", spatial_transform = c(10, 0, 500, 0, -10, 900), spatial_shape = c(4L, 4L))
  ms$add_level("1", derived_from = "0", scale = c(2, 2),
               spatial_transform = c(20, 0, 500, 0, -20, 900), spatial_shape = c(2L, 2L))
  atts <- ms$register(list())
  atts$multiscales <- ms$as_list()
  atts$`spatial:dimensions` <- c("y", "x")
  m <- root$metadata
  m$attributes <- c(atts, group_atts)
  root$metadata <- m
  root$save()

  for (lvl in 0:1) {
    n <- 4L / 2L^lvl
    x <- array(as.numeric(seq_len(n * n)), c(n, n))
    if (group_layout) {
      grp <- root$add_group(as.character(lvl))
      write_plain_array(grp, x, "data", c("y", "x"))
      write_plain_array(grp, array(as.numeric(seq_len(n)), n), "x", "x")
    } else
      write_plain_array(root, x, as.character(lvl), c("y", "x"))
  }
  fn
}

test_that("multiscales group exposes its layout", {
  z <- open_zarr(multiscales_store())
  root <- z[["/"]]
  expect_s3_class(root, "geozarr_group")
  expect_s3_class(root$multiscales, "zarr_convention_multiscales")
  expect_identical(root$multiscales$assets, c("0", "1"))
})

test_that("group without multiscales has no layout", {
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  root <- z[["/"]]
  m <- root$metadata
  m$attributes <- zarr_conv_spatial$new()$register(list())
  root$metadata <- m
  root$save()
  expect_null(open_zarr(fn)[["/"]]$multiscales)
})

test_that("arrays in group levels take the grid of their level", {
  z <- open_zarr(multiscales_store())
  a0 <- z[["/0/data"]]
  a1 <- z[["/1/data"]]
  expect_s3_class(a0, "geozarr_array")
  expect_s3_class(a1, "geozarr_array")
  expect_equal(a0$coordinate_system$axes$x$coordinates$values, c(505, 515, 525, 535))
  expect_equal(a1$coordinate_system$axes$x$coordinates$values, c(510, 530))
  expect_equal(a1$coordinate_system$axes$y$coordinates$values, c(890, 870))
})

test_that("arrays without the spatial dimensions are not part of a level", {
  z <- open_zarr(multiscales_store())
  expect_false(inherits(z[["/0/x"]], "geozarr_array"))
})

test_that("array levels take the grid of their level", {
  z <- open_zarr(multiscales_store(group_layout = FALSE))
  expect_s3_class(z[["/1"]], "geozarr_array")
  expect_equal(z[["/0"]]$coordinate_system$axes$x$coordinates$values, c(505, 515, 525, 535))
  expect_equal(z[["/1"]]$coordinate_system$axes$x$coordinates$values, c(510, 530))
})

test_that("level grid prevails over spatial:transform on the multiscales group", {
  fn <- multiscales_store(group_layout = FALSE,
                          group_atts = list(`spatial:transform` = c(10, 0, 500, 0, -10, 900)))
  z <- open_zarr(fn)
  expect_equal(z[["/1"]]$coordinate_system$axes$x$coordinates$values, c(510, 530))
})

test_that("invalid multiscales attribute is ignored with a warning", {
  fn <- multiscales_store()
  z <- open_zarr(fn)
  root <- z[["/"]]
  m <- root$metadata
  m$attributes$multiscales <- list(layout = list(list(asset = "/0")))
  root$metadata <- m
  root$save()
  expect_warning(z2 <- open_zarr(fn), 'Ignoring "multiscales"')
  expect_null(suppressWarnings(z2[["/"]]$multiscales))
})
