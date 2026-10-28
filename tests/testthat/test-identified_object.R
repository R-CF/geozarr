# IdentifiedObject (internal ancestor class)

IdentifiedObject <- asNamespace("geozarr")$IdentifiedObject

test_that("IdentifiedObject stores name and attributes", {
  io <- IdentifiedObject$new("obj", attributes = list(a = 1))
  expect_identical(io$name, "obj")
  expect_identical(io$getName(), "obj")
  expect_identical(io$attributes, list(a = 1))
})

test_that("IdentifiedObject name and attributes are read-only", {
  io <- IdentifiedObject$new("obj")
  io$name <- "other"
  io$attributes <- list(b = 2)
  expect_identical(io$name, "obj")
  expect_identical(io$attributes, list())
})

test_that("IdentifiedObject aliases", {
  io <- IdentifiedObject$new("obj")
  expect_identical(io$getAlias(), list())
  expect_invisible(io$setAlias(c("o1", "o2")))
  expect_identical(io$getAlias(), c("o1", "o2"))
  expect_error(io$setAlias(1), "character vector")
  expect_error(io$setAlias(c("a", "")), "character vector")
})

test_that("IdentifiedObject remarks", {
  io <- IdentifiedObject$new("obj")
  expect_identical(io$getRemarks(), NA_character_)
  io$setRemarks("note")
  expect_identical(io$getRemarks(), "note")
  expect_error(io$setRemarks(c("a", "b")), "character string")
  expect_error(io$setRemarks(""), "character string")
})

test_that("IdentifiedObject identifiers", {
  io <- IdentifiedObject$new("obj")
  expect_identical(io$getIdentifiers(), list())
  io$setIdentifiers("EPSG:4326")
  expect_identical(io$getIdentifiers(), "EPSG:4326")
  expect_error(io$setIdentifiers(4326), "character vector")
})
