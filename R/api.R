#' Convert an R object into a GeoZarr array
#'
#' This function creates a GeoZarr object from an R matrix or array. A GeoZarr
#' object is like a Zarr object but with special attributes to establish a
#' coordinate system. Default settings will be taken from the R object (data
#' type, shape). Data is chunked into chunks of length 100 (or less if the array
#' is smaller) and compressed.
#'
#' Depending on the properties of the R object, the GeoZarr object may use the
#' "spatial" or "cs" convention for encoding. The "spatial" encoding is the most
#' compact and it will be used for R objects that have at least X and Y
#' dimensions, identified by the names set on the dimensions, and an optional
#' third axis which is typically an image band or a discrete (class) axis -- the
#' third axis may not represent height/depth (Z) or time (T). While the
#' "spatial" convention does work on Zarr arrays with more dimensions, there is
#' no mechanism to attach coordinates to any additional axes. The coordinates
#' must be numeric and regularly spaced and the Y coordinates must be
#' decreasing. In other words, the "spatial" convention will be used for imagery
#' style, north-up arrays with a coordinate system tied to the top-left corner
#' of the array space. For all other cases the "cs" convention will be used.
#'
#' If the coordinates along the axes (the `dimnames` of the R object) are not
#' regularly spaced, secondary Zarr arrays will be created in the same group as
#' the main Zarr array with the axis coordinates, if the length of the axis is
#' longer than the option `GeoZarr.options$max_explicit` -- shorter sets of
#' coordinates are stored in the Zarr array `cs` attributes.
#'
#' Any time coordinates will be converted to a `CFtime` format with a reference
#' of "days since 1970-01-01", compatible with the standard system clock.
#'
#' For more exacting requirements, you should manually construct the GeoZarr
#' object from R objects.
#' @param x The R object to convert. Must be a matrix or array of a numeric or
#'   logical type.
#' @param name Optional. The name of the GeoZarr array to be created. If
#'   omitted, an array will be created at the root of the Zarr store.
#' @param location Optional. If supplied, either an existing [zarr_group] in a
#'   [zarr] object, or a character string giving the location on a local file
#'   system where to persist the data. If the argument is a `zarr_group`,
#'   argument `name` must be provided. If the argument gives the location for a
#'   new Zarr store then the location must be writable by the calling code. As
#'   per the Zarr specification, it is recommended to use a location that ends
#'   in ".zarr" when providing a location for a new store. If argument `name` is
#'   given then the `geozarr_array` will be created in the root of the `zarr`
#'   store with that name. If the `name` argument is not given, a single-array
#'   Zarr store will be created. If the `location` argument is not given, a
#'   `zarr` object is created in memory.
#' @param registration Either "pixel" (the default) or "node". Pixel
#'   registration interprets the coordinates in the "dimnames" of argument `x`
#'   as being the upper-left corner of each grid cell. Node registration
#'   interprets them as the centers of grid cells. In both cases the elements in
#'   the array are assumed to represent an area.
#' @return If the `location` argument is a `zarr_group`, the new `geozarr_array`
#'   instance is returned. Otherwise, the `zarr` object that is newly created
#'   and which contains the GeoZarr array in the root group, or an error if the
#'   `zarr` object could not be created.
#' @docType methods
#' @export
#' @examples
#' x <- array(1:400, c(5, 20, 4))
#' dimnames(x) <- list(x = 100000 + 0:4 * 10000, y = 19:0 * 5000, cls = letters[1:4])
#' z <- as_geozarr(x, "my_data")
#' z
as_geozarr <- function(x, name = NULL, location = NULL, registration = 'pixel') {
  if (is.null(coordinates <- dimnames(x)))
    stop('Can only convert a matrix or array with dimnames set to a GeoZarr object', call. = FALSE)

  if (missing(name) || !nzchar(name))
    name <- NULL

  # Check that required attributes and dimnames are set
  axis_names <- names(coordinates)
  axes <- lapply(axis_names, function(nm) {
    # Abbreviation
    abbr <- unname(.common_axis_abbr[tolower(nm)])
    if (is.na(abbr)) abbr <- 'OTHER'

    # Values -> coordinates -> axis
    v <- suppressWarnings(as.numeric(coordinates[[nm]]))
    len <- length(v)
    if (any(is.na(v))) {
      if (!requireNamespace('CFtime', quietly = TRUE))
        stop('You must install package `CFtime` for this functionality.', call. = FALSE)
      t <- try(CFtime::CFtime('days since 1970-01-01', 'proleptic_gregorian', coordinates[[nm]]), silent = TRUE)
      if (inherits(t, 'try-error')) {
        # String axis
        cv <- CoordinateValuesString$new(coordinates[[nm]])
        coords <- Coordinates$new(name = paste0(nm, '_coordinates'),
                                  direction = 'OTHER', unit = '-', values = cv)
      } else {
        # Time axis
        v <- t$offsets
        if (length(v) > 1L) {
          delta <- diff(v)
          cv <- if (length(v) == 2L || all(abs(diff(delta)) < 0.00001))
            CoordinateValuesNumericPacked$new(c(v[1L], delta[1L]), length(v))
          else
            CoordinateValuesNumeric$new(v)
          coords <- CoordinatesTime$new(name = paste0(nm, '_coordinates'),
                                        direction = if (delta[1L] > 0) 'FUTURE' else 'PAST',
                                        unit = 'days', epoch = '1970-01-01', values = cv)
        } else {
          cv <- CoordinateValuesNumeric$new(v)
          coords <- CoordinatesTime$new(name = paste0(nm, '_coordinates'),
                                        direction = 'OTHER', unit = 'days',
                                        epoch = '1970-01-01', values = cv)
        }
      }
    } else {
      # Numeric axis
      v <- signif(v, digits = 7)
      cv <- if (length(v) > 2L && all(abs(diff(delta <- diff(v))) < 0.00001))
              CoordinateValuesNumericPacked$new(c(v[1L], delta[1L]), length(v))
            else
              CoordinateValuesNumeric$new(v)
      coords <- Coordinates$new(name = paste0(nm, '_coordinates'),
                                direction = 'OTHER', unit = '-', values = cv)
    }

    CoordinateSystemAxis$new(nm, abbr, coords)
  })
  names(axes) <- vapply(axes, function(ax) ax$name, FUN.VALUE = character(1), USE.NAMES = FALSE)
  cs <- CoordinateSystem$new('coordinate_system', axes)

  ax_abbr <- vapply(axes, function(ax) ax$abbreviation, FUN.VALUE = character(1), USE.NAMES = FALSE)
  if (anyDuplicated(ax_abbr) > 0L)
    stop('Duplicate axes detected', call. = FALSE)

  # Make a generic zarr array
  z <- zarr::as_zarr(x, name, location)
  arr <- if (inherits(z, 'zarr')) z[[paste0('/', name)]] else z

  meta <- .geozarr_set_convention(arr$metadata, cs, '..', registration)
  arr$metadata <- meta
  arr$save()

  # Write any external axis coordinate data
  .write_external_coordinates(meta, cs, arr)

  # Prepare the output
  if (inherits(location, 'zarr_group')) {
    gza <- geozarr_array$new(name = name, metadata = meta, parent = location, store = location$store, coord_sys = cs)
    gza$write(x)
    location$set_node(gza)
  } else if (is.character(location)) {
    zarr::open_zarr(location)
  } else {
    # Memory store: replace the zarr_array by a geozarr_array.
    if (is.null(name)) {
      # The root in z is the zarr_array
      gza <- geozarr_array$new(name = '', metadata = meta, store = z$store, coord_sys = cs)
      z$root <- gza
    } else {
      # Replace z[['/name']] (deeper nesting uses 'location' which is covered above)
      gza <- geozarr_array$new(name = name, metadata = meta, parent = z$root, store = z$store, coord_sys = cs)
      z$root$set_node(gza)
    }
    gza$write(x)
    z
  }
}
