# Testing the as_geozarr() function

# The following scenarios are tested:
# 1. name = missing, location = missing   -> memory store, array in root
# 2. name = 'abc',   location = missing   -> memory store, array in '/abc'
# 3. name = missing, location = file path -> local file system store, array in root
# 4. name = 'abc',   location = file path -> local file system store, array in '/abc'
# 5. name = 'abc',   location = group     -> memory or local file system store, array in '/grp/abc'

# For the `cs` convention, both regular and irregular coordinates are tested.

# Basic array with predictable values to use for testing
# dim_names are set in each test
arr <- array(dim = c(5, 20, 4))
for (x in 1:5) {
  for (y in 1:20) {
    for (t in 1:4) arr[x, y, t] <- t * 1000 + y * 10 + x
  }
}
arr_bare <- arr # No dim_names so values can be compared

test_that("spatial-memory", {
  # Inverted Y coordinates, third axis is string class
  dimnames(arr) <- list(x = 100000 + 0:4 * 10000, y = 19:0 * 5000, cls = sprintf("band-%02d", 1:4))

  # Scenario 1
  z <- as_geozarr(x = arr)
  expect_equal(z$arrays, "/")
  expect_true(inherits(z[["/"]], "geozarr_array"))
  expect_identical(z[["/"]][], arr_bare)
  expect_equal(length(z[["/"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "cls"))
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/"]]$coordinate_system$axes[["y"]]$coordinates$values, 19:0 * 5000)
  expect_equal(z[["/"]]$coordinate_system$axes[["cls"]]$coordinates$values, sprintf("band-%02d", 1:4))

  # Scenario 2
  z <- as_geozarr(x = arr, name = "abc")
  expect_equal(z$arrays, "/abc")
  expect_true(inherits(z[["/abc"]], "geozarr_array"))
  expect_identical(z[["/abc"]][], arr_bare)
  expect_equal(length(z[["/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "cls"))
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 19:0 * 5000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["cls"]]$coordinates$values, sprintf("band-%02d", 1:4))

  # Scenario 5
  z <- create_zarr()
  grp <- z$add_group("/", "grp")
  gza <- as_geozarr(x = arr, name = "abc", location = grp)
  expect_equal(z$arrays, "/grp/abc")
  expect_true(inherits(gza, "geozarr_array"))
  expect_identical(gza[], arr_bare)
  expect_true(inherits(z[["/grp/abc"]], "geozarr_array"))
  expect_identical(z[["/grp/abc"]][], arr_bare)
  expect_equal(length(z[["/grp/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "cls"))
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 19:0 * 5000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["cls"]]$coordinates$values, sprintf("band-%02d", 1:4))
})

test_that("spatial-filesystem", {
  # Inverted Y coordinates, third axis is string class
  dimnames(arr) <- list(x = 100000 + 0:4 * 10000, y = 19:0 * 5000, cls = sprintf("band-%02d", 1:4))

  # Scenario 3
  fn <- tempfile(fileext = ".zarr")
  z <- as_geozarr(x = arr, location = fn)
  expect_equal(z$arrays, "/")
  expect_true(inherits(z[["/"]], "geozarr_array"))
  expect_identical(z[["/"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/"]][], arr_bare)
  expect_equal(length(z[["/"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "cls"))
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/"]]$coordinate_system$axes[["y"]]$coordinates$values, 19:0 * 5000)
  expect_equal(z[["/"]]$coordinate_system$axes[["cls"]]$coordinates$values, sprintf("band-%02d", 1:4))
  unlink(fn)

  # Scenario 4
  fn <- tempfile(fileext = ".zarr")
  z <- as_geozarr(x = arr, name = "abc", location = fn)
  expect_equal(z$arrays, "/abc")
  expect_true(inherits(z[["/abc"]], "geozarr_array"))
  expect_identical(z[["/abc"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/abc"]][], arr_bare)
  expect_equal(length(z[["/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "cls"))
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 19:0 * 5000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["cls"]]$coordinates$values, sprintf("band-%02d", 1:4))
  unlink(fn)

  # Scenario 5
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  grp <- z$add_group("/", "grp")
  gza <- as_geozarr(x = arr, name = "abc", location = grp)
  expect_equal(z$arrays, "/grp/abc")
  expect_true(inherits(gza, "geozarr_array"))
  expect_identical(gza[], arr_bare)
  expect_true(inherits(z[["/grp/abc"]], "geozarr_array"))
  expect_identical(z[["/grp/abc"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/grp/abc"]][], arr_bare)
  expect_equal(length(z[["/grp/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "cls"))
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 19:0 * 5000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["cls"]]$coordinates$values, sprintf("band-%02d", 1:4))
  unlink(fn)
})

test_that("cs-regular-memory", {
  # Regular time coordinates
  dimnames(arr) <- list(x = 100000 + 0:4 * 10000, y = 0:19 * 5000, time = sprintf("2026-01-%02d", 1:4))

  # Scenario 1
  z <- as_geozarr(x = arr)
  expect_equal(z$arrays, "/")
  expect_true(inherits(z[["/"]], "geozarr_array"))
  expect_identical(z[["/"]][], arr_bare)
  expect_equal(length(z[["/"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-01-%02d", 1:4))

  # Scenario 2
  z <- as_geozarr(x = arr, name = "abc")
  expect_equal(z$arrays, "/abc")
  expect_true(inherits(z[["/abc"]], "geozarr_array"))
  expect_identical(z[["/abc"]][], arr_bare)
  expect_equal(length(z[["/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-01-%02d", 1:4))

  # Scenario 5
  z <- create_zarr()
  grp <- z$add_group("/", "grp")
  gza <- as_geozarr(x = arr, name = "abc", location = grp)
  expect_equal(z$arrays, "/grp/abc")
  expect_true(inherits(gza, "geozarr_array"))
  expect_identical(gza[], arr_bare)
  expect_true(inherits(z[["/grp/abc"]], "geozarr_array"))
  expect_identical(z[["/grp/abc"]][], arr_bare)
  expect_equal(length(z[["/grp/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-01-%02d", 1:4))
})

test_that("cs-regular-filesystem", {
  # Regular time coordinates
  dimnames(arr) <- list(x = 100000 + 0:4 * 10000, y = 0:19 * 5000, time = sprintf("2026-01-%02d", 1:4))

  # Scenario 3
  fn <- tempfile(fileext = ".zarr")
  z <- as_geozarr(x = arr, location = fn)
  expect_equal(z$arrays, "/")
  expect_true(inherits(z[["/"]], "geozarr_array"))
  expect_identical(z[["/"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/"]][], arr_bare)
  expect_equal(length(z[["/"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-01-%02d", 1:4))
  unlink(fn)

  # Scenario 4
  fn <- tempfile(fileext = ".zarr")
  z <- as_geozarr(x = arr, name = "abc", location = fn)
  expect_equal(z$arrays, "/abc")
  expect_true(inherits(z[["/abc"]], "geozarr_array"))
  expect_identical(z[["/abc"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/abc"]][], arr_bare)
  expect_equal(length(z[["/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-01-%02d", 1:4))
  unlink(fn)

  # Scenario 5
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  grp <- z$add_group("/", "grp")
  gza <- as_geozarr(x = arr, name = "abc", location = grp)
  expect_equal(z$arrays, "/grp/abc")
  expect_true(inherits(gza, "geozarr_array"))
  expect_identical(gza[], arr_bare)
  expect_true(inherits(z[["/grp/abc"]], "geozarr_array"))
  expect_identical(z[["/grp/abc"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/grp/abc"]][], arr_bare)
  expect_equal(length(z[["/grp/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-01-%02d", 1:4))
  unlink(fn)
})

test_that("cs-irregular-memory", {
  # Force external storage of axis coordinates
  old_max_explicit <- geozarr_options()[["max_explicit"]]
  geozarr_options("max_explicit", 3L)

  # Irregular time coordinates: 31 - 28 - 31 days apart
  dimnames(arr) <- list(x = 100000 + 0:4 * 10000, y = 0:19 * 5000, time = sprintf("2026-%02d-01", 1:4))

  # Scenario 2
  z <- as_geozarr(x = arr, name = "abc")
  expect_equal(z$arrays, c("/abc", "/time"))
  expect_true(inherits(z[["/abc"]], "geozarr_array"))
  expect_true(inherits(z[["/time"]], "zarr_array"))
  expect_equal(CFtime::CFTime$new("days since 1970-01-01", "proleptic_gregorian", z[["/time"]][])$format(), sprintf("2026-%02d-01", 1:4))
  expect_identical(z[["/abc"]][], arr_bare)
  expect_equal(length(z[["/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-%02d-01", 1:4))

  # Scenario 5
  z <- create_zarr()
  grp <- z$add_group("/", "grp")
  gza <- as_geozarr(x = arr, name = "abc", location = grp)
  expect_equal(z$arrays, c("/grp/abc", "/grp/time"))
  expect_true(inherits(z[["/grp/time"]], "zarr_array"))
  expect_equal(CFtime::CFTime$new("days since 1970-01-01", "proleptic_gregorian", z[["/grp/time"]][])$format(), sprintf("2026-%02d-01", 1:4))
  expect_true(inherits(gza, "geozarr_array"))
  expect_identical(gza[], arr_bare)
  expect_true(inherits(z[["/grp/abc"]], "geozarr_array"))
  expect_identical(z[["/grp/abc"]][], arr_bare)
  expect_equal(length(z[["/grp/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-%02d-01", 1:4))

  geozarr_options("max_explicit", old_max_explicit)
})

test_that("cs-irregular-filesystem", {
  # Force external storage of axis coordinates
  old_max_explicit <- geozarr_options()[["max_explicit"]]
  geozarr_options("max_explicit", 3L)

  # Irregular time coordinates: 31 - 28 - 31 - 30 days apart
  dimnames(arr) <- list(x = 100000 + 0:4 * 10000, y = 0:19 * 5000, time = sprintf("2026-%02d-01", 1:4))

  # Scenario 4
  fn <- tempfile(fileext = ".zarr")
  z <- as_geozarr(x = arr, name = "abc", location = fn)
  expect_equal(z$arrays, c("/abc", "/time"))
  expect_true(inherits(z[["/abc"]], "geozarr_array"))
  expect_true(inherits(z[["/time"]], "zarr_array"))
  expect_equal(CFtime::CFTime$new("days since 1970-01-01", "proleptic_gregorian", z[["/time"]][])$format(), sprintf("2026-%02d-01", 1:4))
  expect_identical(z[["/abc"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/abc"]][], arr_bare)
  expect_identical(z[["/time"]][], z2[["/time"]][])
  expect_equal(length(z[["/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-%02d-01", 1:4))
  unlink(fn)

  # Scenario 5
  fn <- tempfile(fileext = ".zarr")
  z <- create_zarr(fn)
  grp <- z$add_group("/", "grp")
  gza <- as_geozarr(x = arr, name = "abc", location = grp)
  expect_equal(z$arrays, c("/grp/abc", "/grp/time"))
  expect_true(inherits(gza, "geozarr_array"))
  expect_true(inherits(z[["/grp/time"]], "zarr_array"))
  expect_equal(CFtime::CFTime$new("days since 1970-01-01", "proleptic_gregorian", z[["/grp/time"]][])$format(), sprintf("2026-%02d-01", 1:4))
  expect_identical(gza[], arr_bare)
  expect_true(inherits(z[["/grp/abc"]], "geozarr_array"))
  expect_identical(z[["/grp/abc"]][], arr_bare)
  z2 <- open_zarr(fn)
  expect_identical(z2[["/grp/abc"]][], arr_bare)
  expect_identical(z[["/grp/time"]][], z2[["/grp/time"]][])
  expect_equal(length(z[["/grp/abc"]]$coordinate_system$axes), 3L)
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE), c("x", "y", "time"))
  expect_equal(vapply(z[["/grp/abc"]]$coordinate_system$axes, function(ax) ax$length, FUN.VALUE = integer(1L), USE.NAMES = FALSE), c(5L, 20L, 4L))
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["x"]]$coordinates$values, 100000 + 0:4 * 10000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["y"]]$coordinates$values, 0:19 * 5000)
  expect_equal(z[["/grp/abc"]]$coordinate_system$axes[["time"]]$coordinates$values, sprintf("2026-%02d-01", 1:4))
  unlink(fn)

  geozarr_options("max_explicit", old_max_explicit)
})
