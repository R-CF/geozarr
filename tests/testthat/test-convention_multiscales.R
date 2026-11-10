# zarr_convention_multiscales

sentinel <- function() {
  ms <- zarr_convention_multiscales$new()
  ms$add_level("r10m")
  ms$add_level("r20m",  derived_from = "r10m", scale = c(2, 2))
  ms$add_level("r60m",  derived_from = "r10m", scale = c(6, 6))
  ms$add_level("r120m", derived_from = "r60m", scale = c(2, 2))
  ms$add_level("r720m", derived_from = "r60m", scale = c(12, 12))
  ms
}

test_that("multiscales convention identity", {
  ms <- zarr_convention_multiscales$new()
  expect_identical(ms$name, "multiscales")
  expect_identical(ms$uuid, "d35379db-88df-4056-af3a-620245f8e347")
  expect_match(ms$schema, "zarr-conventions/multiscales/refs/tags/v0.1/schema.json$")
  expect_match(ms$spec, "zarr-conventions/multiscales/blob/v0.1/README.md$")
})

test_that("multiscales as_list() requires a level", {
  expect_error(zarr_convention_multiscales$new()$as_list(), "`add_level\\(\\)`")
})

test_that("multiscales add_level() builds the layout", {
  ms <- zarr_convention_multiscales$new()
  expect_invisible(ms$add_level("0"))
  ms$add_level("1", derived_from = "0", scale = c(2, 2), resampling_method = "average")
  expect_identical(ms$assets, c("0", "1"))
  expect_identical(ms$as_list(),
                   list(layout = list(list(asset = "0"),
                                      list(asset = "1", derived_from = "0",
                                           transform = list(scale = list(2, 2), translation = list(0, 0)),
                                           resampling_method = "average"))))
})

test_that("multiscales default resampling method", {
  ms <- zarr_convention_multiscales$new()
  ms$add_level("0")
  ms$resampling_method <- "average"
  expect_identical(ms$as_list()$resampling_method, "average")
  expect_error(ms$resampling_method <- 1, "character string")
})

test_that("multiscales add_level() validates the asset", {
  ms <- zarr_convention_multiscales$new()
  ms$add_level("0")
  expect_error(ms$add_level("0"), "`asset`")
  expect_error(ms$add_level("/1"), "`asset`")
  expect_error(ms$add_level("../1"), "`asset`")
  expect_error(ms$add_level("1//data"), "`asset`")
})

test_that("multiscales add_level() validates derived_from and scale", {
  ms <- zarr_convention_multiscales$new()
  ms$add_level("0")
  expect_error(ms$add_level("1", derived_from = "9", scale = c(2, 2)), "`derived_from`")
  expect_error(ms$add_level("1", scale = c(2, 2)), "only be given with `derived_from`")
  expect_error(ms$add_level("1", derived_from = "0"), "`scale`")
  expect_error(ms$add_level("1", derived_from = "0", scale = c(2, -2)), "`scale`")
  ms$add_level("1", derived_from = "0", scale = c(2, 2))
  expect_error(ms$add_level("2", derived_from = "1", scale = 2), "same length")
})

test_that("multiscales add_level() checks scale against spatial_transform", {
  ms <- zarr_convention_multiscales$new()
  ms$add_level("0", spatial_transform = c(10, 0, 500, 0, -10, 900))
  expect_error(ms$add_level("1", derived_from = "0", scale = c(4, 4),
                            spatial_transform = c(20, 0, 500, 0, -20, 900)),
               "ratio of the cell sizes")
  expect_identical(ms$assets, "0")
  ms$add_level("1", derived_from = "0", scale = c(2, 2),
               spatial_transform = c(20, 0, 500, 0, -20, 900))
  expect_identical(ms$assets, c("0", "1"))
})

test_that("multiscales cumulative_scale() multiplies along derived_from", {
  cs <- sentinel()$cumulative_scale()
  expect_identical(rownames(cs), c("r10m", "r20m", "r60m", "r120m", "r720m"))
  expect_equal(cs[, 1L], c(r10m = 1, r20m = 2, r60m = 6, r120m = 12, r720m = 72))
  expect_equal(cs[, 1L], cs[, 2L])
})

test_that("multiscales round-trips through as_list()", {
  ms <- sentinel()
  expect_identical(zarr_convention_multiscales$new(ms$as_list())$as_list(), ms$as_list())
})

test_that("multiscales keeps unknown layout properties", {
  ms <- zarr_convention_multiscales$new(list(layout = list(list(asset = "0", `dggs:refinement_level` = 10L))))
  expect_identical(ms$layout[[1L]]$`dggs:refinement_level`, 10L)
})

test_that("multiscales rejects a misformed object", {
  expect_error(zarr_convention_multiscales$new("0"), "misformed")
  expect_error(zarr_convention_multiscales$new(list(layout = list())), "misformed")
  expect_error(zarr_convention_multiscales$new(list(layout = list(list(asset = "/0")))), "misformed")
})

test_that("multiscales rejects an ill-defined derived_from tree", {
  lvl <- function(asset, from) list(asset = asset, derived_from = from, transform = list(scale = c(2, 2)))
  expect_error(zarr_convention_multiscales$new(list(layout = list(list(asset = "0"), list(asset = "0")))),
               "more than once")
  expect_error(zarr_convention_multiscales$new(list(layout = list(list(asset = "0"), lvl("1", "9")))),
               "not an asset")
  expect_error(zarr_convention_multiscales$new(list(layout = list(lvl("a", "b"), lvl("b", "a")))),
               "cycle")
})

test_that("multiscales warns when scale disagrees with spatial:transform", {
  lvl <- function(asset, from, scale, res)
    list(asset = asset, derived_from = from, transform = list(scale = c(scale, scale)),
         `spatial:transform` = c(res, 0, 500, 0, -res, 900))
  layout <- list(list(asset = "0", `spatial:transform` = c(10, 0, 500, 0, -10, 900)),
                 lvl("1", "0", 2, 20), lvl("2", "1", 4, 40))
  expect_warning(zarr_convention_multiscales$new(list(layout = layout)), "level\\(s\\): 2$")
})

test_that("multiscales register() registers spatial only when used", {
  uuids <- function(ms) vapply(ms$register(list())$zarr_conventions, function(c) c$uuid, character(1L))
  ms <- zarr_convention_multiscales$new()
  ms$add_level("0")
  expect_identical(uuids(ms), "d35379db-88df-4056-af3a-620245f8e347")
  ms$add_level("1", derived_from = "0", scale = c(2, 2), spatial_shape = c(2L, 2L))
  expect_identical(uuids(ms), c("d35379db-88df-4056-af3a-620245f8e347",
                                "689b58e2-cf7b-45e0-9fff-9cfc0883d6b4"))
})
