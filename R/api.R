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
#' compact and it will be used for R objects that have at most 2 dimensions,
#' which must be identifiable X and Y axis. While the "spatial" convention does
#' work on Zarr arrays with more dimensions, there is no mechanism to attach
#' coordinates to any additional axes. The coordinates must be numeric and
#' regularly spaced and the Y coordinates must be decreasing. In other words,
#' the "spatial" convention will be used for imagery style, north-up arrays with
#' a coordinate system tied to the top-left corner of the array space. For all
#' other cases the "cs" convention will be used.
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

  # Check that required attributes and dimnames are set
  # Must have named dimensions
  valid_names <- c('x', 'lon', 'longitude', 'easting', 'y', 'lat', 'latitude', 'northing', 'z', 'depth', 'height', 't', 'time')
  abbreviation   <- c('X', 'X', 'X', 'X', 'Y', 'Y', 'Y', 'Y', 'Z', 'Z', 'Z', 'T', 'T')
  axis_names <- names(coordinates)
  axes <- lapply(axis_names, function(nm) {
    # Name and direction
    ndx <- which(valid_names == tolower(nm))
    abbr <- if (length(ndx)) abbreviation[ndx] else 'O' # Use O as sentinel abbreviation for OTHER

    # Values
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
  ax_abbr <- vapply(axes, function(ax) ax$abbreviation, FUN.VALUE = character(1), USE.NAMES = FALSE)
  if (anyDuplicated(ax_abbr) > 0L)
    stop('Duplicate axes detected', call. = FALSE)
  names(axes) <- ax_abbr
  xy <- sum(match(ax_abbr, c('X', 'Y'), nomatch = 0L) > 0L)
  if (xy == 0L)
    stop('Cannot convert to GeoZarr: No X and/or Y axes found.', call. = FALSE)

  # Make a generic zarr array
  z <- zarr::as_zarr(x, name, location)
  arr <- if (inherits(z, 'zarr')) z[[paste0('/', name)]] else z

  # dimension_names
  meta <- append(arr$metadata, list(dimension_names = sapply(axes, function(ax) ax$name)))
  arr$metadata <- meta

  # Set GeoZarr convention attributes
  atts <- meta$attributes %||% list()
  if (xy == 2L && !('Z' %in% ax_abbr) && !('T' %in% ax_abbr) && length(ax_abbr) <= 3L &&
      axes[['X']]$data_type == 'numeric' && axes[['Y']]$data_type == 'numeric' &&
      axes[['X']]$data_arr == 'regular' && axes[['Y']]$data_arr == 'regular' &&
      axes[['Y']]$data_values[2L] < 0) {
    # X + Y, optionally a band, no others, and X + Y coordinates are numeric and regular: spatial convention
    spatial <- zarr_conv_spatial$new()
    atts <- spatial$register(atts)

    dimensions <- c(axes[['Y']]$name, axes[['X']]$name)
    spatial$dimensions <- dimensions
    spatial$set_coordinates(shape = c(axes[['X']]$length, axes[['Y']]$length),
                            x = axes[['X']]$data_values, y = axes[['Y']]$data_values,
                            registration = registration)

    atts <- spatial$write(atts)
  } else {
    # cs convention
    # At least 1 of X, Y, any others
    cs_conv <- zarr_conv_cs$new()
    atts    <- cs_conv$register(atts)

    # Direction lookup by axis abbreviation
    cs_direction <- c(X = 'EAST', Y = 'NORTH', Z = 'UP', T = 'FUTURE', O = 'OTHER')

    axis_defs <- lapply(axes, function(ax) {
      # Values
      dv <- ax$data_values
      values_def <- if (ax$data_arr == 'regular')
        .cs_values_regular(dv[1L], dv[2L])
      else if (is.null(location) || ax$length <= 30L) # FIXME: Make GeoZarr.option
        .cs_values_explicit(dv)
      else {
        # Write coordinate values to a sibling array in the same group as the
        # data array. The sibling name is <axis_name>_coord. The ref path is
        # relative to the data array (its sibling), so just the bare name.
        sibling_name <- paste0(ax$name, '_coord')
        grp <- if (inherits(location, 'zarr_group')) location else z[['/']]
        ab <- zarr::define_array(data_type = if (ax$data_type == 'character') 'string' else 'float64',
                                 shape = ax$length)
        sibling_arr <- grp$add_array(sibling_name, ab)
        sibling_arr[] <- dv
        .cs_values_external(sibling_name)
      }

      # Time
      time_def <- if (ax$data_type == 'time')
        .cs_time(unit = 'days', epoch = '1970-01-01', calendar = 'proleptic_gregorian')
      else
        NULL

      # Coordinates and axis
      coords_def <- .cs_coordinates(values_def, unit = NULL, time = time_def)
      direction  <- cs_direction[[ax$abbreviation]]
      abbr <- ax$abbreviation
      if (abbr == 'O') abbr <- ''
      .cs_axis(list(coords_def), abbreviation = abbr, direction = direction)
    })
    names(axis_defs) <- vapply(axes, function(ax) ax$name, FUN.VALUE = character(1), USE.NAMES = FALSE)

    # Group axes into separate CRS objects by axis category
    cs_conv$add_crs(axes = axis_defs[vapply(axes[ax_abbr %in% c('X', 'Y')], function(ax) ax$name, FUN.VALUE = character(1))])
    vert <- axes[['Z']]
    if (!is.null(vert))
      cs_conv$add_crs(axes = axis_defs[vert$name])
    temp <- axes[['T']]
    if (!is.null(temp))
      cs_conv$add_crs(axes = axis_defs[temp$name])
    other <- axes[['O']]
    if (!is.null(other))
      cs_conv$add_crs(axes = axis_defs[other$name])

    atts <- cs_conv$write(atts)
  }
  meta$attributes <- atts
  arr$metadata <- meta
  arr$save()

  # Prepare the output
  if (inherits(location, 'zarr_group'))
    geozarr_array$new(name, meta, location, location$store)
  else if (is.character(location))
    zarr::open_zarr(location)
  else {
    # Memory store: create a new memory store with the GeoZarr array in or as the root
    # FIXME: Secondary arrays?
    st <- zarr::zarr_memorystore$new()
    if (is.null(name))
      st$create_array(name = '', metadata = meta)
    else {
      st$create_group(name = '')
      st$create_array(parent = '', name = name, metadata = meta)
    }
    zarr::zarr$new(st)
  }
}
