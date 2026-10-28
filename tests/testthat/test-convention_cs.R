# zarr_convention_cs builders

test_that("cs convention identity", {
  cs <- zarr_convention_cs$new()
  expect_identical(cs$uuid, "e4dbf0b7-7a00-4ce6-b23e-484292014ab4")
  expect_match(cs$schema, "zarr_conventions_cs/refs/heads/main/schema.json$")
  expect_identical(cs$crs, list())
  expect_null(cs$cs_crs)
})

test_that("cs values_regular()", {
  cs <- zarr_convention_cs$new()
  expect_identical(cs$values_regular(90, -0.5), list(regular = c(90, -0.5)))
  expect_error(cs$values_regular(c(1, 2), 1), "`start`")
  expect_error(cs$values_regular("0", 1), "`start`")
  expect_error(cs$values_regular(0, 0), "non-zero")
  expect_error(cs$values_regular(0, c(1, 2)), "non-zero")
})

test_that("cs values_explicit()", {
  cs <- zarr_convention_cs$new()
  expect_identical(cs$values_explicit(c(1, 4, 9)), list(explicit = list(1, 4, 9)))
  expect_identical(cs$values_explicit("a"), list(explicit = list("a")))
  expect_error(cs$values_explicit(numeric(0)), "non-empty")
})

test_that("cs values_external()", {
  cs <- zarr_convention_cs$new()
  expect_identical(cs$values_external("../lat"), list(external = list(ref = list(node = "../lat"))))
  expect_identical(cs$values_external("/lat", "s3://bucket/store.zarr"),
                   list(external = list(ref = list(uri = "s3://bucket/store.zarr", node = "/lat"))))
  expect_error(cs$values_external(""), "`node`")
})

test_that("cs boundaries_regular()", {
  cs <- zarr_convention_cs$new()
  expect_identical(cs$boundaries_regular(-0.5, 0.5), list(regular = c(-0.5, 0.5)))
  expect_identical(cs$boundaries_regular(0, 1), list(regular = c(0, 1)))
  expect_error(cs$boundaries_regular(0.5, 1), "non-positive")
  expect_error(cs$boundaries_regular(-1, -0.5), "non-negative")
  expect_error(cs$boundaries_regular(0, 0), "cannot both be 0")
})

test_that("cs time()", {
  cs <- zarr_convention_cs$new()
  expect_identical(cs$time("days", "1970-01-01"),
                   list(unit = "days", epoch = "1970-01-01", calendar = "proleptic_gregorian"))
  expect_identical(cs$time("hours", "2000-01-01", c("360_day", "x"))$calendar, "360_day")
  expect_error(cs$time("", "1970-01-01"), "`unit`")
  expect_error(cs$time("days", 1970), "`epoch`")
  expect_error(cs$time("days", "1970-01-01", ""), "`calendar`")
})

test_that("cs parametric()", {
  cs <- zarr_convention_cs$new()
  terms <- list(eta = cs$values_external("../zeta"), depth_c = cs$values_explicit(10))
  expect_identical(cs$parametric("ocean_s_coordinate_g2", terms),
                   list(formula = "ocean_s_coordinate_g2", terms = terms))
  expect_error(cs$parametric("", terms), "`formula`")
  expect_error(cs$parametric("f", list()), "`terms`")
  expect_error(cs$parametric("f", list(10)), "`terms`")
  expect_error(cs$parametric("f", list(a = list(foo = 1))), "Malformed")
})

test_that("cs coordinates()", {
  cs <- zarr_convention_cs$new()
  v <- cs$values_regular(0, 1)
  expect_identical(cs$coordinates(v), list(values = v))
  expect_identical(cs$coordinates(v, name = "lon", unit = "degrees_east",
                                  boundaries = cs$boundaries_regular(-0.5, 0.5),
                                  attributes = list(long_name = "Longitude")),
                   list(values = v, name = "lon", unit = "degrees_east",
                        boundaries = list(regular = c(-0.5, 0.5)),
                        attributes = list(long_name = "Longitude")))
})

test_that("cs coordinates() drops dimensionless and time units", {
  cs <- zarr_convention_cs$new()
  v <- cs$values_regular(0, 1)
  expect_null(cs$coordinates(v, unit = "-")$unit)
  tm <- cs$time("days", "1970-01-01")
  crd <- cs$coordinates(v, unit = "days", time = tm)
  expect_null(crd$unit)
  expect_identical(crd$time, tm)
})

test_that("cs coordinates() validates arguments", {
  cs <- zarr_convention_cs$new()
  v <- cs$values_regular(0, 1)
  expect_error(cs$coordinates(list(foo = 1)), "malformed")
  expect_error(cs$coordinates("x"), "malformed")
  expect_error(cs$coordinates(v, name = ""), "`name`")
  expect_error(cs$coordinates(v, unit = c("m", "km")), "`unit`")
})

test_that("cs axis()", {
  cs <- zarr_convention_cs$new()
  crd <- list(cs$coordinates(cs$values_regular(0, 1)))
  expect_identical(cs$axis(crd), list(coordinates = crd))
  expect_identical(cs$axis(crd, "X", "east"), list(coordinates = crd, abbreviation = "X", direction = "EAST"))
  expect_error(cs$axis(list()), "non-empty list")
  expect_error(cs$axis(crd, c("X", "Y")), "`abbreviation`")
  expect_error(cs$axis(crd, direction = "sideways"), "AxisDirection")
})

test_that("cs add_crs() accumulates CRS objects", {
  cs <- zarr_convention_cs$new()
  ax <- cs$axis(list(cs$coordinates(cs$values_regular(0, 1))), "X", "EAST")
  expect_null(cs$add_crs(NULL))
  expect_null(cs$add_crs(list()))
  expect_invisible(cs$add_crs(list(lon = ax), type = "planar"))
  cs$add_crs(list(time = ax), type = "temporal", id = zarr_conv_proj$new(list(code = "OGC:1")),
             geolocation = list(foo = 1))
  expect_length(cs$crs, 2L)
  expect_identical(cs$crs[[1L]], list(type = "planar", axes = list(lon = ax)))
  expect_identical(cs$crs[[2L]]$id, list(`proj:code` = "OGC:1"))
  expect_identical(cs$crs[[2L]]$geolocation, list(foo = 1))
})

test_that("cs add_crs() validates axes", {
  cs <- zarr_convention_cs$new()
  ax <- cs$axis(list(cs$coordinates(cs$values_regular(0, 1))))
  expect_error(cs$add_crs(list(ax)), "named list")
  expect_error(cs$add_crs(list(lon = ax, ax)), "named list")
  expect_error(cs$add_crs(list(lon = 1)), "non-empty list")
  expect_error(cs$add_crs(list(lon = list(abbreviation = "X"))), "non-empty list")
})

test_that("cs as_list()", {
  cs <- zarr_convention_cs$new()
  expect_error(cs$as_list(), "add_crs")
  ax <- cs$axis(list(cs$coordinates(cs$values_regular(0, 1))))
  cs$add_crs(list(lon = ax))
  expect_identical(cs$as_list()$crs, list(list(type = "unknown", axes = list(lon = ax))))
  expect_error(cs$cs_name <- "", "non-empty")
})

test_that("cs coordinate set name is independent of the convention name", {
  cs <- zarr_convention_cs$new()
  ax <- cs$axis(list(cs$coordinates(cs$values_regular(0, 1))))
  cs$add_crs(list(lon = ax))
  expect_identical(names(cs$as_list()), "crs")
  cs$cs_name <- "my_cs"
  expect_identical(cs$as_list()$name, "my_cs")
  expect_identical(cs$register(list())$zarr_conventions[[1L]]$name, "cs")
})

test_that("cs cs_crs only accepts a proj convention", {
  cs <- zarr_convention_cs$new()
  cs$cs_crs <- "EPSG:4326"
  expect_null(cs$cs_crs)
  p <- zarr_conv_proj$new(list(code = "EPSG:4326"))
  cs$cs_crs <- p
  expect_identical(cs$cs_crs, p)
})

test_that("cs as_list() includes the compound CRS id", {
  cs <- zarr_convention_cs$new()
  cs$add_crs(list(lon = cs$axis(list(cs$coordinates(cs$values_regular(0, 1))))))
  cs$cs_crs <- zarr_conv_proj$new(list(code = "EPSG:4326"))
  expect_identical(cs$as_list()$id, list(`proj:code` = "EPSG:4326"))
})
