#' Convert an R object into a GeoZarr array
#'
#' This function creates a GeoZarr object from an R matrix or array. A GeoZarr
#' object is like a Zarr object but with special attributes to establish a
#' coordinate system. Default settings will be taken from the R object (data
#' type, shape). Data is chunked into chunks of length 100 (or less if the array
#' is smaller) and compressed.
#'
#' Depending on the properties of the R object, the GeoZarr object may use the
#' "spatial:" or "crs" convention for encoding. The "spatial:" encoding is the
#' most compact and it will be used for R objects that have at most 3
#' dimensions, which must include an identifiable X and Y axis and which do not
#' include a Z or T axis, with any third axis being non-spatial (categorical).
#' The coordinates must be numeric and regularly spaced. For all other
#' cases the "crs" convention will be used, including for time and vertical
#' axes.
#'
#' If the coordinates along the axes (the `dimnames` of the R object) are not
#' regularly spaced, secondary Zarr arrays will be created with the axis
#' coordinates. Any time coordinates will be converted to a `CFtime` format with
#' a reference of "days since 1970-01-01", compatible with the standard system
#' clock.
#'
#' For more exacting requirements, you should manually construct the GeoZarr
#' object from R objects.
#' @param x The R object to convert. Must be a matrix or array of a numeric or
#'   logical type.
#' @param name The name of the GeoZarr array to be created.
#' @param location Optional. If supplied, either an existing [zarr_group] in a
#'   Zarr object, or a character string giving the location on a local file
#'   system where to persist the data. If the argument gives the location for a
#'   new Zarr store then the location must be writable by the calling code. As
#'   per the Zarr specification, it is recommended to use a location that ends
#'   in ".zarr" when providing a location for a new store. If the `location`
#'   argument is not given, a Zarr object is created in memory.
#' @param registration Either "pixel" (the default) or "node". Pixel
#'   registration interprets the coordinates in the "dimnames" of argument `x`
#'   as being the upper-left corner of each grid cell. Node registration
#'   interprets them as the centers of grid cells. In all cases the elements in
#'   the array are assumed to represent an area.
#' @return If the `location` argument is a `zarr_group`, the new GeoZarr array
#'   is returned. Otherwise, the GeoZarr object that is newly created and which
#'   contains the GeoZarr array, or an error if the GeoZarr object could not be
#'   created.
#' @docType methods
#' @export
#' @examples
#' x <- array(1:400, c(5, 20, 4))
#' dimnames(x) <- list(x = 100000 + 0:4 * 10000, y = 0:19 * 5000, cls = letters[1:4])
#' z <- as_geozarr(x, "my_data")
#' z
as_geozarr <- function(x, name, location = NULL, registration = 'node') {
  if (is.null(coordinates <- dimnames(x)))
    stop('Can only convert a matrix or array with dimnames set to a GeoZarr object', call. = FALSE)

  # Check that required attributes and dimnames are set
  # Must have named dimensions
  valid_names <- c('x', 'lon', 'longitude', 'easting', 'y', 'lat', 'latitude', 'northing', 'z', 'depth', 'height', 't', 'time', 'band')
  abbreviation   <- c('X', 'X', 'X', 'X', 'Y', 'Y', 'Y', 'Y', 'Z', 'Z', 'Z', 'T', 'T', '')
  axis_names <- names(coordinates)
  axes <- lapply(axis_names, function(nm) {
    # name and direction
    ndx <- which(valid_names == tolower(nm))
    abbr <- if (length(ndx)) abbreviation[ndx] else ''

    # values
    v <- suppressWarnings(as.numeric(coordinates[[nm]]))
    len <- length(v)
    if (any(is.na(v))) {
      if (!requireNamespace('CFtime', quietly = TRUE))
        stop('You must install package `CFtime` for this functionality.', call. = FALSE)
      t <- try(CFtime::CFtime('days since 1970-01-01', 'proleptic_gregorian', coordinates[[nm]]), silent = TRUE)
      if (inherits(t, 'try-error')) {
        dt <- 'character'
        dseq <- 'explicit'
        dv <- coordinates[[nm]]
      } else {
        dt <- 'time'
        v <- t$offsets
        if (length(v) > 1L) {
          delta <- diff(v)
          if (length(v) == 2L || all(abs(diff(delta)) < 0.00001)) {
            dseq <- 'regular'
            dv <- c(v[1L], delta[1L])
          } else {
            dseq <- 'explicit'
            dv <- v
          }
        } else {
          dseq <- 'explicit'
          dv <- v
        }
      }
    } else {
      dt <- 'numeric'
      v <- signif(v, digits = 7)
      if (length(v) > 1L) {
        delta <- diff(v)
        if (length(v) == 2L || all(abs(diff(delta)) < 0.00001)) {
          dseq <- 'regular'
          dv <- c(v[1L], delta[1L])
        } else {
          dseq <- 'explicit'
          dv <- v
        }
      } else {
        dseq <- 'explicit'
        dv <- v
      }
    }

    # output
    list(name = nm, abbreviation = abbr, length = len, data_type = dt, data_arr = dseq, data_values = dv)
  })
  ax_abbr <- sapply(axes, function(ax) ax$abbreviation)
  # FIXME: Check for duplicates of X, Y, Z, T
  names(axes) <- ax_abbr

  # Make a generic zarr object in memory
  z <- zarr::as_zarr(x, name, location)
  arr <- z[[paste0('/', name)]]

  # dimension_names
  meta <- append(arr$metadata, list(dimension_names = sapply(axes, function(ax) ax$name)))
  arr$metadata <- meta

  # Set GeoZarr convention attributes
  atts <- meta$attributes %||% list()
  xy <- sum(match(ax_abbr, c('X', 'Y'), nomatch = 0L) > 0L)
  if (xy == 0L)
    stop('Cannot convert to GeoZarr: No X and/or Y axes found.', call. = FALSE)

  if (xy == 2L && !('Z' %in% ax_abbr) && !('T' %in% ax_abbr) && length(ax_abbr) <= 3L &&
      axes[['X']]$data_type == 'numeric' && axes[['Y']]$data_type == 'numeric' &&
      axes[['X']]$data_arr == 'regular' && axes[['Y']]$data_arr == 'regular') {
    # X + Y, optionally a band, no others, and X + Y coordinates are numeric and regular: spatial convention
    spatial <- zarr_conv_spatial$new()
    atts <- spatial$register(atts)

    # Modify Y coordinates, if necessary
    yvals <- as.numeric(coordinates[[axes[['Y']]$name]])
    if (yvals[axes[['Y']]$length] > yvals[1L])
      axes[['Y']]$data_values <- c(yvals[axes[['Y']]$length], -axes[['Y']]$data_values[2L])

    dimensions <- c(axes[['Y']]$name, axes[['X']]$name)
    spatial$dimensions <- dimensions
    spatial$set_coordinates(shape = c(axes[['X']]$length, axes[['Y']]$length),
                            x = axes[['X']]$data_values,
                            y = axes[['Y']]$data_values,
                            z = NULL, registration = registration)

    atts <- spatial$write(atts)
  } else {
    # cs conventions
    # At least 1 of X, Y, any others

  }
  meta$attributes <- atts
  arr$metadata <- meta
  arr$save()

  z
}
