# Extracted from test-api.R:252

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "geozarr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
arr <- array(dim = c(5, 20, 4))
for (x in 1:5) {
  for (y in 1:20) {
    for (t in 1:4) arr[x, y, t] <- t * 1000 + y * 10 + x
  }
}
arr_bare <- arr

# test -------------------------------------------------------------------------
old_max_explicit <- geozarr_options()[["max_explicit"]]
geozarr_options("max_explicit", 3L)
dimnames(arr) <- list(x = 100000 + 0:4 * 10000, y = 0:19 * 5000, time = sprintf("2026-%02d-01", 1:4))
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
z <- create_zarr()
grp <- z$add_group("/", "grp")
gza <- as_geozarr(x = arr, name = "abc", location = grp)
expect_equal(z$arrays, c("/grp/abc", "/grp/time"))
