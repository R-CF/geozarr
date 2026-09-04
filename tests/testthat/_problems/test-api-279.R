# Extracted from test-api.R:279

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
fn <- tempfile(fileext = ".zarr")
z <- as_geozarr(x = arr, name = "abc", location = fn)
