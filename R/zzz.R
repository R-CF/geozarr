#' @importFrom zarr zarr_register_domain
#' @importFrom R6 R6Class
NULL

#nocov start
# Create environments for package settings and options
GeoZarr.options <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Register this profile with zarr
  zarr::zarr_register_domain(zarr_domain_geozarr$new())

  # Register the conventions specific to GeoZarr
  assign("conventions", data.frame(
    name   = c("spatial:"),
    schema = c("https://raw.githubusercontent.com/zarr-conventions/spatial/refs/tags/v1/schema.json"),
    uuid   = c("689b58e2-cf7b-45e0-9fff-9cfc0883d6b4")
  ), envir = GeoZarr.options)
}

.onUnload <- function(libname) {
  # Unregister this profile with zarr
  zarr::zarr_unregister_domain("GeoZarr")
}
#nocov end
