# Internal helpers in R/utils.R

gz <- asNamespace("geozarr")

test_that("geozarr_options() returns all options", {
  opts <- geozarr_options()
  expect_type(opts, "list")
  expect_true(all(c("eps", "max_explicit", "conventions") %in% names(opts)))
  expect_s3_class(opts$conventions, "data.frame")
  expect_true(all(c("cs", "spatial", "proj", "geolocation") %in% opts$conventions$name))
})

test_that("geozarr_options() sets max_explicit and eps", {
  local_geozarr_option("max_explicit", 7.9)
  expect_identical(geozarr_options()$max_explicit, 7L)
  local_geozarr_option("eps", c(1e-6, 1))
  expect_identical(geozarr_options()$eps, 1e-6)
})

test_that("geozarr_options() ignores non-numeric values and unknown keys", {
  old <- geozarr_options()
  geozarr_options("max_explicit", "a")
  geozarr_options("eps", NULL)
  geozarr_options("no_such_key", 1)
  expect_identical(geozarr_options()$max_explicit, old$max_explicit)
  expect_identical(geozarr_options()$eps, old$eps)
  expect_null(geozarr_options()$no_such_key)
})

test_that(".near() tolerates rounding but not real differences", {
  expect_true(gz$.near(0.1 + 0.2, 0.3))
  expect_true(gz$.near(0, 1e-13))
  expect_false(gz$.near(1, 1.001))
  expect_identical(gz$.near(c(1, 2), c(1, 3)), c(TRUE, FALSE))
})

test_that(".monotonicity() classifies vectors", {
  expect_identical(gz$.monotonicity(1:5), 1L)
  expect_identical(gz$.monotonicity(5:1), -1L)
  expect_identical(gz$.monotonicity(c(1, 3, 2)), 0L)
  expect_identical(gz$.monotonicity(c(1, 1, 2)), 0L)     # strictly
  expect_identical(gz$.monotonicity(c(1, NA, 3)), 1L)    # NA ignored
})

test_that(".is_regular() detects constant spacing", {
  expect_true(gz$.is_regular(c(0, 2, 4, 6)))
  expect_true(gz$.is_regular(c(6, 4, 2)))
  expect_false(gz$.is_regular(c(0, 1, 3)))
  expect_false(gz$.is_regular(5))
})

test_that(".slim.data.frame() truncates long strings and flattens lists", {
  df <- data.frame(a = c("short", strrep("x", 60)), b = 1:2)
  df$l <- list(c("p", "q"), "r")
  out <- gz$.slim.data.frame(df, width = 20L)
  expect_identical(names(out), c("a", "b", "l"))
  expect_identical(unname(out$a), c("short", paste0(strrep("x", 17), "...")))
  expect_identical(out$b, 1:2)
  expect_identical(unname(out$l), c("p, q", "r"))
})

test_that(".common_axis_abbr maps common names", {
  abbr <- gz$.common_axis_abbr
  expect_identical(unname(abbr[c("lon", "latitude", "plev", "time", "band")]),
                   c("X", "Y", "Z", "T", "other"))
  expect_false(anyDuplicated(names(abbr)) > 0L)
})

# .conventions_supported() ------------------------------------------------------

conv_meta <- function(...) list(attributes = list(zarr_conventions = list(...)))
cs_cmo      <- list(name = "cs", uuid = "e4dbf0b7-7a00-4ce6-b23e-484292014ab4")
spatial_cmo <- list(name = "spatial", uuid = "689b58e2-cf7b-45e0-9fff-9cfc0883d6b4")
proj_cmo    <- list(name = "proj",
                    schema_url = "https://raw.githubusercontent.com/zarr-conventions/proj/refs/tags/v0.1/schema.json")

test_that(".conventions_supported() returns NA without conventions", {
  expect_identical(gz$.conventions_supported(list(attributes = list())), NA_character_)
})

test_that(".conventions_supported() identifies the principal convention", {
  expect_identical(gz$.conventions_supported(conv_meta(cs_cmo)), "cs")
  expect_identical(gz$.conventions_supported(conv_meta(spatial_cmo, proj_cmo)), "spatial")
  expect_identical(gz$.conventions_supported(conv_meta(spatial_cmo, cs_cmo)), "cs")
  expect_identical(gz$.conventions_supported(conv_meta(proj_cmo)), NA_character_)
})

test_that(".conventions_supported() errors on unidentifiable conventions", {
  expect_error(gz$.conventions_supported(conv_meta(list(name = "x", uuid = "nope"))),
               "unsupported conventions: x")
  # Name alone is not a discovery field
  expect_error(gz$.conventions_supported(conv_meta(list(name = "cs"))),
               "unsupported conventions: cs")
  expect_error(gz$.conventions_supported(conv_meta(list(uuid = "nope"))),
               "unsupported conventions: nope")
})
