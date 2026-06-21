#' @importFrom zarr zarr_register_domain
#' @importFrom R6 R6Class
NULL

#nocov start
# Create environments for package settings and options
GeoZarr.options <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Register this profile with zarr
  zarr::zarr_register_domain(zarr_domain_geozarr$new())

  assign('eps', .Machine$double.eps^0.5, envir = GeoZarr.options)
  assign('max_explicit', 30L, envir = GeoZarr.options)

  # Register the conventions supported by GeoZarr (and Zarr)
  assign('conventions', rbind(data.frame(
    name   = c('cs', 'spatial', 'proj', 'geolocation'),
    schema = c('https://raw.githubusercontent.com/R-CF/zarr_convention_cs/main/schema.json',
               'https://raw.githubusercontent.com/zarr-conventions/spatial/refs/tags/v1/schema.json',
               'https://raw.githubusercontent.com/zarr-conventions/geo-proj/refs/tags/v1/schema.json',
               'https://raw.githubusercontent.com/R-CF/zarr_convention_geolocation/refs/heads/main/schema.json'),
    uuid   = c('e4dbf0b7-7a00-4ce6-b23e-484292014ab4',
               '689b58e2-cf7b-45e0-9fff-9cfc0883d6b4',
               'f17cb550-5864-4468-aeb7-f3180cfb622f',
               'bb9ee930-8c60-4c47-ad6b-8daa558987ed')
  ), zarr::zarr_conventions()), envir = GeoZarr.options)
}

.onUnload <- function(libname) {
  # Unregister this profile with zarr
  zarr::zarr_unregister_domain('GeoZarr')
}
#nocov end
