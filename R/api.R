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
    stop('Can only convert a matrix or array to a GeoZarr object when dimnames are set', call. = FALSE)

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
    crds <- if (any(is.na(v))) .make_coordinate_values(coordinates[[nm]])
            else .make_coordinate_values(v)
    coords <- switch(crds$mode,
      'integer' = ,
      'double' = {
        if (!is.null(crds$length))
          CoordinatesPacked$new(name = paste0(nm, '_coordinates'),
                                direction = 'OTHER', unit = '-', values = crds$values, length = crds$length)
        else
          Coordinates$new(name = paste0(nm, '_coordinates'),
                          direction = 'OTHER', unit = '-', values = crds$values)
      },
      'character' = {
        if (!requireNamespace('CFtime', quietly = TRUE))
          stop('You must install package `CFtime` for this functionality', call. = FALSE)
        t <- try(CFtime::CFtime('days since 1970-01-01', 'proleptic_gregorian', crds$values), silent = TRUE)
        if (inherits(t, 'try-error'))
          # String axis
          CoordinatesString$new(name = paste0(nm, '_coordinates'),
                                direction = 'OTHER', unit = '-', values = crds$values)
        else {
          # Time axis
          off <- t$offsets
          dir <- if (length(crds$values) == 1L) 'OTHER'
                 else if (off[2L] > off[1L]) 'FUTURE'
                 else 'PAST'
          CoordinatesTime$new(name = paste0(nm, '_coordinates'), direction = dir,
                              unit = 'days', epoch = '1970-01-01', values = off)
        }
      }
    )
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
  meta <- set_convention(arr$metadata, cs, external_group = '..', registration = registration)
  arr$metadata <- meta
  arr$save()

  # Prepare the output
  if (inherits(location, 'zarr_group')) {
    gza <- geozarr_array$new(name = name, metadata = meta, parent = location, store = location$store, coord_sys = cs)
    gza$write_external_coordinates()
    location$set_node(gza)
  } else if (is.character(location)) {
    if (is.null(name)) {
      gza <- geozarr_array$new(name = '', metadata = meta, store = z$store, coord_sys = cs)
      z$root <- gza
    } else {
      gza <- geozarr_array$new(name = name, metadata = meta, parent = z$root, store = z$store, coord_sys = cs)
      z$root$set_node(gza)
    }
    gza$write_external_coordinates()
    z
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
    gza$write_external_coordinates()
    z
  }
}

#' Create a new GeoZarr array
#'
#' @description This function creates a new GeoZarr array in an existing Zarr
#'   store. Only the structure of the array will be created; data and other
#'   properties like attributes have to be added through additional method
#'   calls. This function is particularly well suited to creating an array that
#'   mimics the structure of a netCDF data variable.
#' @details The `axes` argument provides all the details of the coordinate
#'   system of the new array in a compound `list`. The elements in the list are
#'   each a `list`, named after the names of the axes. The number of axes listed
#'   in the top-level list determines the rank of the new array: its number of
#'   dimensions. The order of the dimensions in the array that will be created
#'   is the same as the order of the axes in this argument. The sub-lists are
#'   structured as follows:
#'   * `coordinates`: A vector of coordinate values. The length of the vector
#'   becomes the length of the corresponding dimension in the array.
#'   * `boundaries`: Optional. For boundaries that are regular, meaning that the
#'   lower offset `(-∞, 0)` for each coordinate is constant and the same for the
#'   higher offset `(0, ∞)`, then a vector of the lower and higher offset, in
#'   that order, may be supplied. Otherwise a matrix has to be supplied with as
#'   many columns as there are coordinates and the lower offsets in row 1 and
#'   the higher offsets in row 2.
#'   * `abbreviation`: A character from the set `(X, Y, Z, T)`. These values
#'   indicate the spatio-temporal dimensions of the array. If omitted, an empty
#'   string or a space character the axis will have no spatio-temporal meaning,
#'   such as for a categorical axis. Note that both "X" and "Y" must be present
#'   in the set of axes. Abbreviations may not be duplicated across axes.
#'   * `direction`: The identifier of the direction of increasing coordinate
#'   values of the axis. Its value must be present in the `AxisDirection` object
#'   in this package. If omitted, the direction will be inferred from the
#'   `abbreviation` and `coordinates` fields of the axis, which is usually
#'   correct.
#'   * `unit`: The unit-of-measure of the coordinate values. While not
#'   mandatory, it is highly recommended to include this information. For a
#'   temporal axis the acceptable values are "second", "minute", "hour", "day"
#'   and "year", as well as the abbreviation or plural version thereof; if
#'   omitted, "days" is used. When temporal coordinates are specific as numeric
#'   offsets, this field is required.
#'   * `calendar`: Temporal axis only. The name of the calendar to use. This
#'   package supports all calendars of the [CF Metadata
#'   Conventions](https://cfconventions.org/cf-conventions/cf-conventions.html#time-coordinate),
#'   including the "model" calendars and leap seconds in the "utc" calendar. If
#'   omitted, "proleptic_gregorian" is used, which is almost identical to the
#'   common calendar used by regular R date-time functions. When temporal
#'   coordinates are specific as numeric offsets, this field is required.
#'   * `epoch`: Temporal axis only. The starting point from which time
#'   coordinates are calculated. Must be specified in ISO 8601 format, but with
#'   support for model calendars. If omitted, "1970-01-01" is used, as in
#'   regular R date-time functions. When temporal coordinates are specific as
#'   numeric offsets, this field is required.
#'   * `chunk_weight`: Optional chunking weight of this axis. Higher values lead
#'   to less fragmentation of the axis into separate chunks compared to lower
#'   values. If omitted, it is set to 1 for this axis. Typical values are in the
#'   range of 1 - 3.
#'
#'   The top-level list may have an additional element called `.convention` that
#'   can hold additional information specific to the convention used for
#'   encoding of the array metadata; see below for details.
#'
#'   Depending on the details of the `coordinates` argument, the GeoZarr object
#'   may use the "spatial" or "cs" convention for encoding.
#'
#'   **"spatial" convention:** The "spatial" convention
#'   encoding is the most compact and it will be used for arrays that have X and
#'   Y axes and an optional third axis which is typically an image band or a
#'   discrete (class) axis -- the third axis may not represent height/depth (Z)
#'   or time (T). While the "spatial" convention does work on Zarr arrays with
#'   more dimensions, it has no mechanism to attach coordinates to any axes
#'   other than X and Y. The X and Y coordinates must be numeric and regularly
#'   spaced and the Y coordinate values must be decreasing. In other words, the
#'   "spatial" convention will be used for imagery style, north-up arrays with a
#'   coordinate system tied to the top-left corner of the array space. The
#'   `axis` sub-list only requires the `coordinates` and `abbreviation` fields
#'   for each of the axes; `chunk_weight` will be used when specified, all other
#'   fields are ignored.
#'
#'   If a `.convention` element is present in the top-level `axes` list then it
#'   is searched for a `registration` member. Its value is a character string
#'   with a value of "node" or "pixel". If omitted, a value of "pixel" is used.
#'
#'   **"cs" convention:** The "cs" convention is used for all cases where the
#'   "spatial" convention does not apply. All fields for the `axis` sub-lists in
#'   the `axes` argument are interpreted.
#'
#'   If the coordinates along an axis are not regularly spaced, a secondary Zarr
#'   array will be created to hold the coordinate values. By default the
#'   secondary array will be stored in the same group as the main array or
#'   another existing group may be indicated. If the length of the axis is no
#'   more than the option `GeoZarr.options$max_explicit` its coordinates are
#'   stored in the array `cs` attributes.
#'
#'   Any time coordinates will be converted to a `CFtime` format with a
#'   user-defined epoch and calendar, or default values can be used to align
#'   with the common calendar. Time coordinates may be specified as a character
#'   vector with coordinates in ISO 8601 format, or as a numeric vector of
#'   offsets from an epoch. In this latter case, the vector may not be packed
#'   and the fields "unit", "calendar" and "epoch" must be given for the
#'   temporal axis. Boundary values may be set around numerical coordinates
#'   (both spatial and temporal).
#'
#'   If a `.convention` element is present in the top-level `axes` list then it
#'   is searched for a `coordinate_group` member. Its value is a character
#'   string giving the relative path from the newly created array to the group
#'   where external coordinate arrays are to be stored. That group must already
#'   exist. If omitted, a value of ".." is used, meaning that any external
#'   coordinate arrays are placed in the same group as the new array.
#' @param name The name of the GeoZarr array to be created.
#' @param location A [zarr_group] instance. The new array will be created in
#'   this group, with the indicated `name`.
#' @param axes A named `list` with the axis information for the array. For every
#'   element in the list an axis will be created in the array with a length that
#'   equals the number of coordinates contained in the list element. See the
#'   Details section and the example for the formatting details of the list.
#' @param data_type Character. The Zarr data type for the array.
#' @param fill_value Optional. The sentinel value used to indicate parts of the
#'   array that do not have data assigned. The value of this argument must agree
#'   with the `data_type`. If not provided a default value will be used that is
#'   specific to the `data_type`.
#' @return The new `geozarr_array` instance.
#' @docType methods
#' @export
#' @examples
#' z <- create_zarr()
#' grp <- z$add_group("/", "my_data_group")
#' ext <- z$add_group("/", "coords")
#' gza <- create_geozarr_array(name = "my_data_array", location = grp,
#'                             data_type = "float32", fill_value = -9e12,
#'                             axes = list(
#'          longitude   = list(coordinates = seq(from = -175, to = 175, by = 10),
#'                             boundaries = rbind(seq(from = -180, to = 170, by = 10),
#'                                                seq(from = -170, to = 180, by = 10)),
#'                             abbreviation = "X",
#'                             unit = "degrees"),
#'          latitude    = list(coordinates = c(-90, -75, -60, -50, -40, -30, -20, -10,
#'                                             0, 10, 20, 30, 40, 50, 60, 75, 90),
#'                             abbreviation = "Y",
#'                             unit = "degrees"),
#'          time        = list(coordinates = sprintf("2026-%02d-%02d",
#'                                                   rep(1:12, each = 30),
#'                                                   rep(1:30, times = 12)),
#'                             abbreviation = "T",
#'                             unit = "days",
#'                             calendar = "360_day",
#'                             epoch = "1850-01-01",
#'                             chunk_weight = 1.5),
#'          .convention = list(coordinate_group = "../../coords")
#'          )
#'        )
#' z$hierarchy()
#' gza
create_geozarr_array <- function(name, location, axes, data_type, fill_value) {
  if (!inherits(location, 'zarr_group'))
    stop('Argument "location" must be a zarr_group instance', call. = FALSE)
  if (!is.list(axes))
    stop('Argument "axes" must be a list with axis descriptions', call. = FALSE)

  # Get .convention, then delete the element
  conv <- axes$.convention
  axes$.convention <- NULL

  axis_names <- names(axes)
  if (is.null(axis_names) || any(!nzchar(axis_names)))
    stop('Argument "axes" must have named elements for each of the axes', call. = FALSE)

  # Chunk weights
  chunking <- vapply(axes, function(ax) ax$chunk_weight %||% 1, numeric(1L), USE.NAMES = FALSE)

  # Build the coordinate system
  ax <- lapply(seq_along(axes), function(i) {
    nm <- axis_names[i]

    # Abbreviation
    abbr <- axes[[i]]$abbreviation
    if (is.null(abbr) || !nzchar(abbr) || abbr == ' ') {
      abbr <- unname(.common_axis_abbr[tolower(nm)])
      if (is.na(abbr)) abbr <- 'OTHER'
    }
    if (!abbr %in% c('X', 'Y', 'Z', 'T', 'OTHER'))
      stop('Bad abbreviation for axis "', nm, '"', call. = FALSE)

    # Direction
    dir <- axes[[i]]$direction
    if (is.null(dir) && length(axes[[i]]$coordinates) < 2L)
      dir <- 'OTHER'
    if (is.null(dir))
      dir <- unname(c('X' = 'EAST', 'Y' = 'NORTH', 'Z' = 'UP', 'T' = 'FUTURE', 'OTHER' = 'OTHER')[abbr])

    # Units, calendar, epoch
    unit <- axes[[i]]$unit
    calendar <- axes[[i]]$calendar
    epoch <- axes[[i]]$epoch

    # Coordinate values
    crds <- axes[[i]]$coordinates
    if (!is.null(unit) && !is.null(calendar) && !is.null(epoch) && is.numeric(crds)) {
      # Catch numeric time early on so that regular coordinates do not get packed
      coords <- CoordinatesTime$new(name = paste0(nm, '_coordinates'), direction = dir,
                                    unit = unit, epoch = epoch, calendar = calendar,
                                    values = crds, bounds = axes[[i]]$boundaries)
    } else {
      crds <- .make_coordinate_values(crds)
      coords <- switch(crds$mode,
        'integer' = ,
        'double' = {
          if (!is.null(crds$length))
            CoordinatesPacked$new(name = paste0(nm, '_coordinates'), direction = dir,
                                  unit = unit %||% '-', values = crds$values,
                                  length = crds$length, bounds = axes[[i]]$boundaries)
          else {
            Coordinates$new(name = paste0(nm, '_coordinates'), direction = dir,
                            unit = unit %||% '-', values = crds$values,
                            bounds = axes[[i]]$boundaries)
          }
        },
        'character' = {
          if (!requireNamespace('CFtime', quietly = TRUE))
            stop('You must install package `CFtime` for this functionality', call. = FALSE)
          epoch <- epoch %||% '1970-01-01'
          calendar <- calendar %||% 'proleptic_gregorian'
          if (is.null(unit) || unit == '-') units <- 'days'
          t <- try(CFtime::CFTime$new(paste(units, 'since', epoch), calendar, crds$values), silent = TRUE)
          if (inherits(t, 'try-error'))
            # String axis
            CoordinatesString$new(name = paste0(nm, '_coordinates'), direction = dir,
                                  unit = '-', values = crds$values)
          else
            # Time axis
            CoordinatesTime$new(name = paste0(nm, '_coordinates'), direction = dir,
                                unit = unit, epoch = epoch, calendar = calendar,
                                values = t$offsets, bounds = axes[[i]]$boundaries)
        }
      )
    }
    CoordinateSystemAxis$new(nm, abbr, coords)
  })
  names(ax) <- axis_names
  shp <- vapply(ax, function(x) x$length, integer(1L), USE.NAMES = FALSE)
  cs <- CoordinateSystem$new('coordinate_system', ax)

  ax_abbr <- vapply(ax, function(x) x$abbreviation, FUN.VALUE = character(1), USE.NAMES = FALSE)
  if (anyDuplicated(ax_abbr) > 0L)
    stop('Duplicate axis abbreviations detected', call. = FALSE)

  # Make the geozarr array
  ab <- zarr::define_array(data_type, shp)
  if (!missing(fill_value))
    ab$fill_value <- fill_value
  ab$chunk_shape <- zarr::optimal_chunking(shp, chunking)
  meta <- set_convention(ab$metadata(), cs, external_group = conv$coordinate_group %||% '..',
                         registration = conv$registration %||% 'pixel')
  meta <- location$store$create_array(location$path, name, meta)
  gza <- geozarr_array$new(name, meta, location, location$store, cs)
  location$set_node(gza)
  gza$write_external_coordinates()

  gza
}
