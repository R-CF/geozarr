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

# Convention matching -----------------------------------------------------------

spatial_schema <- "https://raw.githubusercontent.com/zarr-conventions/spatial/refs/tags/v0.1/schema.json"
spatial_spec   <- "https://github.com/zarr-conventions/spatial/blob/v0.1/README.md"

test_that(".url_version() splits off a version segment", {
  expect_identical(gz$.url_version(spatial_schema),
                   list(base = "https://raw.githubusercontent.com/zarr-conventions/spatial/{ref}/schema.json",
                        version = c(0L, 1L, 0L)))
  expect_identical(gz$.url_version("https://x.org/a/1.2.3/s.json")$version, c(1L, 2L, 3L))
  expect_identical(gz$.url_version("https://x.org/a/v2/s.json")$version, c(2L, 0L, 0L))
})

test_that(".url_version() treats refs/tags and short refs alike", {
  expect_identical(gz$.url_version(spatial_schema)$base,
                   gz$.url_version(sub("refs/tags/", "", spatial_schema))$base)
})

test_that(".url_version() gives a GitHub branch reference no version", {
  u <- "https://raw.githubusercontent.com/R-CF/zarr_conventions_cs/refs/heads/main/schema.json"
  expect_identical(gz$.url_version(u),
                   list(base = "https://raw.githubusercontent.com/R-CF/zarr_conventions_cs/{ref}/schema.json",
                        version = NULL))
})

test_that(".url_version() gives tag and branch references the same identity", {
  expect_identical(gz$.url_version(spatial_spec)$base,
                   gz$.url_version(sub("v0.1", "main", spatial_spec))$base)
})

test_that(".url_version() leaves unversioned non-GitHub URLs whole", {
  expect_identical(gz$.url_version("https://x.org/a/schema.json"),
                   list(base = "https://x.org/a/schema.json", version = NULL))
})

test_that(".versions_compatible() follows semantic versioning", {
  expect_true(gz$.versions_compatible(c(1L, 0L, 0L), c(1L, 4L, 2L)))
  expect_false(gz$.versions_compatible(c(1L, 0L, 0L), c(2L, 0L, 0L)))
  expect_true(gz$.versions_compatible(c(0L, 1L, 0L), c(0L, 1L, 3L)))
  expect_false(gz$.versions_compatible(c(0L, 1L, 0L), c(0L, 2L, 0L)))   # MAJOR 0: MINOR breaks
  expect_true(gz$.versions_compatible(NULL, c(0L, 2L, 0L)))
})

test_that(".match_convention() identifies by uuid, not by name", {
  expect_identical(gz$.match_convention(list(uuid = "689b58e2-cf7b-45e0-9fff-9cfc0883d6b4")), "spatial")
  expect_identical(gz$.match_convention(list(name = "spatial")), NA_character_)
  # A present uuid decides identity, even when the schema would match
  expect_identical(gz$.match_convention(list(uuid = "nope", schema_url = spatial_schema)), NA_character_)
})

test_that(".match_convention() checks the version of a uuid match", {
  uuid <- "689b58e2-cf7b-45e0-9fff-9cfc0883d6b4"
  expect_identical(gz$.match_convention(list(uuid = uuid, schema_url = sub("v0.1", "v0.1.2", spatial_schema))), "spatial")
  expect_identical(gz$.match_convention(list(uuid = uuid, schema_url = sub("v0.1", "v1", spatial_schema))), NA_character_)
  expect_identical(gz$.match_convention(list(uuid = uuid, spec_url = sub("v0.1", "v0.2", spatial_spec))), NA_character_)
})

test_that(".match_convention() accepts a relocated convention by uuid", {
  uuid <- "689b58e2-cf7b-45e0-9fff-9cfc0883d6b4"
  moved <- "https://raw.githubusercontent.com/some-org/spatial/refs/tags/v0.1/schema.json"
  expect_identical(gz$.match_convention(list(uuid = uuid, schema_url = moved)), "spatial")
  expect_identical(gz$.match_convention(list(schema_url = moved)), NA_character_)
})
test_that(".match_convention() matches by schema, with compatible versions", {
  expect_identical(gz$.match_convention(list(schema_url = spatial_schema)), "spatial")
  expect_identical(gz$.match_convention(list(schema_url = sub("v0.1", "v0.1.4", spatial_schema))), "spatial")
  expect_identical(gz$.match_convention(list(schema_url = sub("v0.1", "v0.2", spatial_schema))), NA_character_)
})

test_that(".match_convention() matches by spec when no schema is given", {
  expect_identical(gz$.match_convention(list(spec_url = spatial_spec)), "spatial")
  expect_identical(gz$.match_convention(list(spec_url = sub("v0.1", "v1", spatial_spec))), NA_character_)
})

test_that(".match_convention() identifies by unversioned URLs", {
  expect_identical(gz$.match_convention(list(spec_url = sub("v0.1", "main", spatial_spec))), "spatial")
  expect_identical(gz$.match_convention(list(schema_url = sub("refs/tags/v0.1", "main", spatial_schema))), "spatial")
})

test_that(".match_convention() takes the version from the schema before the spec", {
  # Unversioned spec: the schema version decides
  expect_identical(gz$.match_convention(list(schema_url = sub("v0.1", "v0.2", spatial_schema),
                                             spec_url = sub("v0.1", "main", spatial_spec))), NA_character_)
  # Unversioned schema: the spec version decides
  expect_identical(gz$.match_convention(list(schema_url = sub("refs/tags/v0.1", "main", spatial_schema),
                                             spec_url = spatial_spec)), "spatial")
})

test_that(".match_convention() knows zarr's conventions", {
  expect_identical(gz$.match_convention(list(uuid = "3bbe438d-df37-49fe-8e2b-739296d46dfb")), "uom")
})
