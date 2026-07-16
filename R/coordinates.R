# The AxisDirection class from the Java implementation is here represented as a character vector.
# The values in this vector are the only allowable values for the direction of a
# Coordinates instance.
AxisDirection <- c(
  'COLUMN_NEGATIVE',  # Axis positive direction is towards lower pixel column.
  'COLUMN_POSITIVE',  # Axis positive direction is towards higher pixel column.
  'DISPLAY_DOWN',     # Axis positive direction is towards bottom of approximately vertical display surface.
  'DISPLAY_LEFT',     # Axis positive direction is left in display.
  'DISPLAY_RIGHT',    # Axis positive direction is right in display.
  'DISPLAY_UP',       # Axis positive direction is towards top of approximately vertical display surface.
  'DOWN',             # Axis positive direction is down relative to gravity.
  'EAST',             # Axis positive direction is π/2 radians clockwise from north.
  'EAST_NORTH_EAST',  # Axis positive direction is approximately east-north-east.
  'EAST_SOUTH_EAST',  # Axis positive direction is approximately east-south-east.
  'FUTURE',           # Axis positive direction is towards the future.
  'GEOCENTRIC_X',     # Axis positive direction is in the equatorial plane from the centre of the modelled earth towards the intersection of the equator with the prime meridian.
  'GEOCENTRIC_Y',     # Axis positive direction is in the equatorial plane from the centre of the modelled earth towards the intersection of the equator and the meridian π/2 radians eastwards from the prime meridian.
  'GEOCENTRIC_Z',     # Axis positive direction is from the centre of the modelled earth parallel to its rotation axis and towards its north pole.
  'NORTH',            # Axis positive direction is north.
  'NORTH_EAST', # Axis positive direction is approximately north-east.
  'NORTH_NORTH_EAST', # Axis positive direction is approximately north-north-east.
  'NORTH_NORTH_WEST', # Axis positive direction is approximately north-north-west.
  'NORTH_WEST',       # Axis positive direction is approximately north-west.
  'OTHER',            # Unknown or unspecified axis orientation.
  'PAST',             # Axis positive direction is towards the past.
  'ROW_NEGATIVE',     # Axis positive direction is towards lower pixel row.
  'ROW_POSITIVE',     # Axis positive direction is towards higher pixel row.
  'SOUTH',            # Axis positive direction is π radians clockwise from north.
  'SOUTH_EAST',       # Axis positive direction is approximately south-east.
  'SOUTH_SOUTH_EAST', # Axis positive direction is approximately south-south-east.
  'SOUTH_SOUTH_WEST', # Axis positive direction is approximately south-south-west.
  'SOUTH_WEST',       # Axis positive direction is approximately south-west.
  'UP',               # Axis positive direction is up relative to gravity.
  'WEST',             # Axis positive direction is 3π/2 radians clockwise from north.
  'WEST_NORTH_WEST',  # Axis positive direction is approximately west-north-west.
  'WEST_SOUTH_WEST')  # Axis positive direction is approximately west-south-west.

# ====== Coordinates ===========================================================

#' Coordinates
#'
#' @description This class implements the coordinates class. The coordinates are
#'   always associated with a coordinate system axis.
#'
#'   This class provides the basic interface for coordinate values and it
#'   manages numeric coordinate values, both integer and floating-point.
#'   Descendant classes manage packed numerical values, character values and
#'   time values.
#'
#'   The optional boundary values for numeric coordinate values that define the
#'   finite extent of each element in the coordinates are also managed by this
#'   class.
#' @docType class
Coordinates <- R6::R6Class('Coordinates',
  cloneable = FALSE,
  private = list(
    .name = NA_character_,
    .direction = NA_character_,
    .unit = NA_character_,

    # The values may be packed or unpacked. If packed, the two values represent
    # the initial value along the coordinate axes and the increment. Values may
    # be numeric, integer or character. If not packed, this is a vector of
    # values as long as there are coordinates along the axis.
    .values = NULL,

    # Optionally, the boundary values for numeric or integer coordinate values.
    # If they are packed, then the field is a vector of two values, the value to
    # subtract from the coordinate values to derive the lower boundary value,
    # and the value to add for the higher boundary value, in that order. If not
    # packed, a matrix with two rows (row 1 = lower, row 2 = higher offset) and
    # as many columns as there are coordinate values.
    .bounds = NULL,

    # This method return a vector of the extreme values of the coordinates. By
    # default the .values are taken. Descendant classes should override this as
    # necessary.
    coordinate_range = function() {
      range(private$.values)
    },

    # This method returns the number of coordinates. By default the length of
    # .values is taken. Descendant classes with packed data should override this
    # as necessary.
    coordinate_length = function() {
      length(private$.values)
    }
  ),
  public = list(
    #' @description Create a new coordinates instance for use with a coordinate
    #'   system axis.
    #' @param name Character string. Name of the coordinate set.
    #' @param direction Character string. Direction of the coordinates. Must be
    #'   one from a set of values.
    #' @param unit Character string. Unit of measure of the coordinates.
    #' @param values A vector of values. If argument `values_packed` is `TRUE`
    #'   then there are two values: the initial coordinate value and the
    #'   increment. Otherwise this is a vector of values.
    #' @param bounds Optional. If the boundaries are regularly spaced, a vector
    #'   with the offset to the boundary lower and higher than the coordinate
    #'   value, respectively. If the boundaries are irregularly spaced, a matrix
    #'   with two rows and as many columns as there are elements, with the
    #'   offset to the boundary below the coordinate value in row 1 and the
    #'   offset to the boundary above the coordinate value in row 2. The
    #'   boundaries represent the finite extent around the boundary value that
    #'   this value is representative for. If this argument is not provided, the
    #'   coordinate values are assumed to represent a point value.
    #' @return An instance of this class or an error.
    initialize = function(name, direction, unit, values, bounds = NULL) {
      if (is.character(name) && length(name) == 1L && nzchar(name))
        private$.name <- name
      else
        stop("Coordinates name must be a character string.", call. = FALSE)

      if (is.character(direction) && length(direction) == 1L && toupper(direction) %in% AxisDirection)
        private$.direction <- direction
      else
        stop('Axis direction must be a character string from `AxisDirection`', call. = FALSE)

      if (is.character(unit) && length(unit) == 1L)
        private$.unit <- unit
      else
        stop('Coordinate unit must be a character string', call. = FALSE)

      if (!is.vector(values))
        stop('Argument `values` must be a vector of coordinate values', call. = FALSE)
      private$.values <- values

      if (is.matrix(bounds) && ncol(bounds) != length(values))
        stop('Boundary values matrix must have as many columns as the length of argument `values`', call. = FALSE)
      if (!missing(bounds) && !is.null(bounds))
        private$.bounds <- bounds
    },

    #' @description Print a summary of the coordinates to the console.
    #' @param ... Ignored.
    #' @return Self, invisible.
    print = function(...) {
      cat('<Coordinates> ', private$.name, '\n', sep = '')
      cat('Length    :', private$coordinate_length, '\n')
      cat('Direction :', private$.direction, '\n')
      vals <- private$coordinate_range()
      cat('Values    : [', vals[1L], ' ... ', vals[2L], '] ',
          if (is.na(private$.unit) || !nzchar(private$.unit)) '-' else private$.unit,
          '\n', sep = '')
      invisible(self)
    },

    #' @description Some details of the axis.
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      nm <- private$.name

      vals <- private$coordinate_range()
      vals <- paste0('[', vals[1L], ' ... ', vals[2L], ']', sep = '')

      data.frame(name = private$.name, direction = private$.direction, values = vals,
                 unit = if (is.na(private$.unit) || !nzchar(private$.unit)) '-' else private$.unit)
    },

    #' @description Return an exact copy of these coordinates.
    #' @return A new instance of `Coordinates`.
    copy = function() {
      Coordinates$new(private$.name, private$.direction, private$.unit, private$.values, private$.bounds)
    },

    #' @description Return coordinates spanning a smaller coordinate range.
    #' @param rng The range of indices to include in the returned coordinates.
    #' @return A new `Coordinates` instance covering the indicated range of
    #'   indices, including boundary values, present.
    subset = function(rng) {
      values <- private$.values[rng[1L]:rng[2L]]
      bounds <- if (is.null(private$.bounds)) NULL
                else if (is.matrix(private$.bounds)) private$.bounds[ , rng[1L]:rng[2L]] # Matrix form
                else private$.bounds                                                     # Packed form
      Coordinates$new(self$name, private$.direction, private$.unit, values, bounds)
    },

    #' @description Given a range of domain coordinate values, returns the
    #'   indices into the axis that fall within the supplied range. If the axis
    #'   has boundary values, any coordinate whose boundary values fall entirely
    #'   or partially within the supplied range will be included in the result.
    #' @param rng A numeric vector whose extreme values indicate the indices of
    #'   coordinates to return.
    #' @return An integer vector of length 2 with the lower and higher indices
    #'   into the axis that fall within the range of coordinates in argument
    #'   `rng`. Returns `NULL` if no (boundary) values of the axis fall within
    #'   the range of coordinates.
    slice = function(rng) {
      rng <- range(rng)
      idx <- if (is.null(private$.bounds)) {
        vals <- self$values # Will unpack packed data
        which(vals >= rng[1L] & vals <= rng[2L])
      } else {
        bnds <- self$bounds # Will unpack packed data
        lo <- pmin(bnds[1L, ], bnds[2L, ])
        hi <- pmax(bnds[1L, ], bnds[2L, ])
        which(hi >= rng[1L] & lo <= rng[2L])
      }
      if (!length(idx)) NULL else as.integer(range(idx))
    },

    #' @description Retrieve the direction of the coordinates. This method is
    #'   mandatory in the OGC standard for an axis.
    #' @return Character string with the direction of the coordinates.
    getDirection = function() {
      private$.direction
    },

    #' @description Retrieve the unit of measure of the coordinates. This method
    #'   is mandatory in the OGC standard for an axis.
    #' @return Character string with the unit of measure of the axis.
    getUnit = function() {
      private$.unit
    },

    #' @description Retrieve the minimum coordinate value of this set of
    #'   coordinates, in units of the unit of measure of the coordinates.
    #' @return Negative infinity.
    getMinimumValue = function() {
      -Inf
    },

    #' @description Retrieve the maximum coordinate value of this set of
    #'   coordinates, in units of the unit of measure of the coordinates.
    #' @return Positive infinity.
    getMaximumValue = function() {
      +Inf
    },

    #' @description Retrieve the interpretation of coordinate values, either as
    #' a direct value ("EXACT") or as a wrap-around value between the minimum
    #' and maximum value of the coordinates ("WRAPAROUND").
    #' @return The character string 'EXACT'.
    getRangeMeaning = function() {
      'EXACT'
    }
  ),
  active = list(
    #' @field name Set or retrieve the name of the coordinate set.
    name = function(value) {
      if (missing(value))
        private$.name
      else if (is.character(value))
        private$.name <- value[1L]
    },

    #' @field direction Set or retrieve the direction of the coordinates.
    direction = function(value) {
      if (missing(value))
        private$.direction
      else if (value[1L] %in% AxisDirection)
        private$.direction <- value[1L]
    },

    #' @field values (read-only) Retrieve the coordinate values as a full vector
    #'   of values.
    values = function(value) {
      if (missing(value))
        private$.values
    },

    #' @field raw (read-only) Retrieve the coordinate values as stored, either a
    #'   full vector or packed.
    raw = function(value) {
      if (missing(value))
        private$.values
    },

    #' @field bounds (read-only) Retrieve the boundary values around
    #'   coordinates.
    bounds = function(value) {
      if (missing(value)) {
        if (is.matrix(private$.bounds))
          private$.bounds
        else if (is.vector(private$.bounds)) {
          vals <- self$values
          rbind(vals - private$.bounds[1L], vals + private$.bounds[2L])
        } else NULL
      }
    },

    #' @field range (read-only) Retrieve the extreme values of the coordinates.
    range = function(value) {
      private$coordinate_range()
    },

    #' @field length (read-only) The number of elements in this instance once
    #'   the values are unpacked.
    length = function(value) {
      if (missing(value))
        private$coordinate_length()
    },

    #' @field unit Character string giving the unit-of-measure of the coordinate
    #'   values.
    unit = function(value) {
      if (missing(value))
        private$.unit
      else if (is.character(value) && length(value) == 1L)
        private$.unit <- value
    }
  )
)

# ====== CoordinatesPacked =====================================================

#' Packed coordinates
#'
#' @description This class implements the packed coordinates class. The
#'   coordinates are always associated with a coordinate system axis.
#'
#'   This class provides the specific interface for packed numeric coordinate
#'   values, both integer and floating-point.
#' @docType class
CoordinatesPacked <- R6::R6Class('CoordinatesPacked',
  inherit = Coordinates,
  cloneable = FALSE,
  private = list(
    # The number of coordinates when unpacked.
    .length = 0L,

    # This method return a vector of the extreme values of the coordinates.
    coordinate_range = function() {
      range(c(private$.values[1L], private$.values[1L] + (private$.length - 1L) * private$.values[2L]))
    },

    # This method returns the number of coordinates when unpacked.
    coordinate_length = function() {
      private$.length
    }
  ),
public = list(
  #' @description Create a new coordinates instance for use with a coordinate
  #'   system axis for regularly spaced coordinate values.
  #' @param name Character string. Name of the coordinate set.
  #' @param direction Character string. Direction of the coordinates. Must be
  #'   one from a set of values.
  #' @param unit Character string. Unit of measure of the coordinates.
  #' @param values A vector of values. There must be two values: the initial
  #'   coordinate value and the increment.
  #' @param length Integer value giving the number of coordinates that this
  #'   packed data is for.
  #' @param bounds Optional. If the boundaries are regularly spaced, a vector
  #'   with the offset to the boundary lower and higher than the coordinate
  #'   value, respectively. If the boundaries are irregularly spaced, a matrix
  #'   with two rows and as many columns as there are elements, with the offset
  #'   to the boundary below the coordinate value in row 1 and the offset to the
  #'   boundary above the coordinate value in row 2. The boundaries represent
  #'   the finite extent around the boundary value that this value is
  #'   representative for. If this argument is not provided, the coordinate
  #'   values are assumed to represent a point value.
  #' @return An instance of this class or an error.
  initialize = function(name, direction, unit, values, length, bounds = NULL) {
    private$.length <- length
    super$initialize(name, direction, unit, values, bounds)
  },

  #' @description Return an exact copy of these coordinates.
  #' @return A new instance of `CoordinatesPacked`.
  copy = function() {
    CoordinatesPacked$new(private$.name, private$.direction, private$.unit, private$.values, private$.length, private$.bounds)
  },

  #' @description Return coordinates spanning a smaller coordinate range.
  #' @param rng The range of indices to include in the returned coordinates.
  #' @return A new `CoordinatesPacked` instance covering the indicated range of
  #'   indices, including boundary values, present.
  subset = function(rng) {
    values <- c(private$.values[1L] + private$.values[2L] * (rng[1L] - 1L), private$.values[2L])
    bounds <- if (is.null(private$.bounds)) NULL
              else if (is.matrix(private$.bounds)) private$.bounds[ , rng[1L]:rng[2L]] # Matrix form
              else private$.bounds                                                     # Packed form
    CoordinatesPacked$new(self$name, private$.direction, private$.unit, values, private$.length, bounds)
  }
),
  active = list(
    #' @field values (read-only) Retrieve the unpacked coordinate values.
    values = function(value) {
      if (missing(value))
        seq(from = private$.values[1L], by = private$.values[2L], length = private$.length)
    },

    #' @field range (read-only) Retrieve the extreme values of the coordinates.
    range = function(value) {
      c(private$.values[1L], private$.values[1L] + (private$.length - 1L) * private$.values[2L])
    }
  )
)

# ====== CoordinatesString =====================================================

#' String-type coordinates
#'
#' @description This class implements the string-type coordinates class. The
#'   coordinates are always associated with a coordinate system axis.
#'
#'   This class provides the specific interface for string-type coordinate
#'   values.
#' @docType class
CoordinatesString <- R6::R6Class('CoordinatesString',
  inherit = Coordinates,
  cloneable = FALSE,
  public = list(
    #' @description Create a new coordinates instance for use with a coordinate
    #'   system axis for string-type coordinate values.
    #' @param name Character string. Name of the coordinate set.
    #' @param direction Character string. Direction of the coordinates. Must be
    #'   one from a set of values.
    #' @param unit Character string. Unit of measure of the coordinates.
    #' @param values A vector of values. There must be two values: the initial
    #'   coordinate value and the increment.
    #' @return An instance of this class or an error.
    initialize = function(name, direction, unit, values) {
      super$initialize(name, direction, unit, values)
    },

    #' @description Return an exact copy of these coordinates.
    #' @return A new instance of `CoordinatesString`.
    copy = function() {
      CoordinatesString$new(private$.name, private$.direction, private$.unit, private$.values)
    },

    #' @description Return coordinates spanning a smaller coordinate range.
    #' @param rng The range of indices to include in the returned coordinates.
    #' @return A new `CoordinatesString` instance covering the indicated range of
    #'   indices.
    subset = function(rng) {
      CoordinatesString$new(self$name, private$.direction, private$.unit, private$.values[rng[1L]:rng[2L]])
    }
  )
)

# ====== CoordinatesTime =======================================================

#' Time coordinates
#'
#' @description This class implements the coordinates class for time. Time
#'   coordinates can use any of the calendars defined by the CF Metadata
#'   Conventions.
#'
#'   This class will store the explicit values of the time coordinate, and
#'   optionally its boundaries, just like other instances of the base class
#'   [Coordinates]. Additionally, it stores an instance of `CFTime`, which
#'   converts the raw values to intelligible coordinates, such as
#'   "2026-06-01T12:04:15".
#'
#' @docType class
CoordinatesTime <- R6::R6Class('CoordinatesTime',
  inherit = Coordinates,
  cloneable = FALSE,
  private = list(
    .time = NULL,

    # Overriding the base method to supply time stamps instead of values.
    coordinate_range = function() {
      private$.time$range()
    },

    # Timestamps in argument `ts` may be a single one, abbreviated, a range, by
    # season/quarter/dekad, etc so rework to two proper timestamps.
    expand_timestamps = function(ts) {
      # Year as numeric: YYYY or YYYY:YYYY
      if (is.numeric(ts)) {
        ts <- range(as.integer(ts))
        return(c(paste0(ts[1L], "-01-01"), paste0(ts[2L] + 1L, "-01-01")))
      }

      if (!is.character(ts))
        stop("Bad format for timestamps.", call. = FALSE)

      if (length(ts) == 1L)
        ts <- c(ts, ts)
      else if (length(ts) != 2L)
        stop("Bad format for timestamps.", call. = FALSE)

      # Year as character string
      if (all(grepl("^[0-9]{4}$", ts)))
        return(c(paste0(ts[1L], "-01-01"), paste0(as.integer(ts[2L]) + 1L, "-01-01")))

      # Year-month as character string
      ym <- utils::strcapture("^([0-9]{4})-(0[1-9]|1[0-2])$", ts, data.frame(year = integer(), month = integer()))
      if (!any(is.na(ym))) {
        if (ym$month[2L] == 12L) {
          ym$year[2L] <- ym$year[2L] + 1L
          ym$month[2L] <- 1L
        } else
          ym$month[2L] <- ym$month[2L] + 1L
        return(sprintf("%04d-%02d-01", ym$year, ym$month))
      }

      # Year-season as character string
      ys <- utils::strcapture("^([0-9]{4})-S([1-4])$", ts, data.frame(year = integer(), season = integer()))
      if (!any(is.na(ys))) {
        if (ys$season[1L] == 1L) {
          ys$year[1L] <- ys$year[1L] - 1L
          ys$season[1L] <- 5L
        }
        ys$season[2L] <- ys$season[2L] + 1L
        return(sprintf("%04d-%02d-01", ys$year, (ys$season - 1L) * 3L))
      }

      # Year-quarter as character string
      yq <- utils::strcapture("^([0-9]{4})-Q([1-4])$", ts, data.frame(year = integer(), quarter = integer()))
      if (!any(is.na(yq))) {
        if (yq$quarter[2L] == 4L) {
          yq$year[2L] <- yq$year[2L] + 1L
          yq$quarter[2L] <- 1L
        } else
          yq$quarter[2L] <- yq$quarter[2L] + 1L
        return(sprintf("%04d-%02d-01", yq$year, (yq$quarter - 1L) * 3L + 1L))
      }

      # Year-dekad as character string
      yk <- utils::strcapture("^([0-9]{4})-D([0-2][1-9]|3[0-6])$", ts, data.frame(year = integer(), dekad = integer()))
      if (!any(is.na(yk))) {
        mod <- yk$dekad %% 3L # which dekad in the month: 1, 2, 0
        if (yk$dekad[2L] == 36L) {
          yk$year[2L] <- yk$year[2L] + 1L
          yk$dekad[2L] <- 1L
        } else if (mod[2L] == 0L) {
          yk$dekad[2L] <- yk$dekad[2L] + 1L
          mod[2L] <- 1L
        } else {
          yk$dekad[2L] <- yk$dekad[2L] + 1L
          mod[2L] <- mod[2L] + 1L
        }
        d <- ifelse(mod == 0L, 21L, (mod - 1L) * 10L + 1L)
        return(sprintf("%04d-%02d-%02d", yk$year, (yk$dekad - 1L) %/% 3L + 1L, d))
      }

      # Year-month-day - only if both dates are identical (so only a single day was specified)
      if (ts[1L] == ts[2L]) {
        ymd <- private$.time$calendar$parse(ts[1L])
        if (is.na(ymd$year))
          stop("Bad format for timestamps: Date not valid in calendar", call. = FALSE)
        ymd <- rbind(ymd, private$.time$calendar$add_day(ymd))
        return(sprintf("%04d-%02d-%02d", ymd$year, ymd$month, ymd$day))
      }

      # If all else fails, just return the passed-in argument
      ts
    }
  ),
  public = list(
    #' @description Create a new coordinates instance for use with a coordinate
    #'   system axis for time.
    #' @param name Character string. Name of the coordinate set.
    #' @param direction Character string. Direction of increasing coordinate
    #'   values. Must be either "FUTURE" or "PAST".
    #' @param unit Character string. Unit of measure of the time coordinates.
    #'   Must be "second", "minute", "hour", "day" or "year", or the
    #'   abbreviation or plural thereof.
    #' @param epoch Character string. An ISO 8601 time stamp providing the
    #'   reference point for time coordinate calculations.
    #' @param calendar Character string, optional. The calendar to use for the
    #'   time coordinates. Must be one of the calendars supported by the CF
    #'   Metadata Conventions. If not given, defaults to "standard".
    #' @param values A vector of values. They may be numeric, in which case they
    #'   are taken to be offsets from the epoch in the given unit, or character,
    #'   in which case they must be ISO 8601 time stamps.
    #' @param bounds Optional. If the boundaries are regularly spaced, a vector
    #'   with the offset to the boundary lower and higher than the coordinate
    #'   value, respectively. If the boundaries are irregularly spaced, a matrix
    #'   with two rows and as many columns as there are elements, with the
    #'   offset to the boundary below the coordinate value in row 1 and the
    #'   offset to the boundary above the coordinate value in row 2. The
    #'   boundaries represent the finite extent around the boundary value that
    #'   this value is representative for. If this argument is not provided, the
    #'   coordinate values are assumed to represent a point value.
    #' @return An instance of this class or an error.
    initialize = function(name, direction, unit, epoch, calendar = 'standard', values, bounds = NULL) {
      super$initialize(name, direction, unit, values, bounds)
      private$.time <- try(CFtime::CFTime$new(definition = paste(unit, 'since', epoch),
                                     calendar = calendar, offsets = values), silent = TRUE)
      if (inherits(private$.time, 'try-error'))
        stop('Arguments do not form a valid calendar definition', call. = FALSE)

      if (!is.null(bounds))
        private$.time$set_bounds(self$bounds)
    },

    #' @description Return an exact copy of these coordinates.
    #' @return A new instance of `CoordinatesTime`.
    copy = function() {
      time_def <- strsplit(private$.time$calendar$definition, ' ', fixed = TRUE)[[1L]]
      bounds <- private$.bounds
      if (!is.null(bounds))
        bounds <- bounds$copy()
      CoordinatesTime$new(private$.name, private$.direction,
                 time_def[1L], time_def[3L], private$.time$calendar$name,
                 private$.values$clone(), bounds)
    },

    #' @description Retrieve the indices of the time axis falling between two
    #'   extreme values.
    #' @param x A vector of two timestamps in between of which all indices into
    #'   the time axis to extract.
    #' @param rightmost.closed Whether or not to include the upper limit.
    #'   Default is `FALSE`.
    #' @return An integer vector giving the indices in the time axis between
    #'   values in `x`, or `NULL` if none of the values are valid.
    slice = function(x, rightmost.closed = FALSE) {
      x <- private$expand_timestamps(x)
      time <- private$.time
      idx <- suppressWarnings(time$slice(x, rightmost.closed))
      if (all(!idx)) NULL
      else range((1L:length(time))[idx])
    }
  ),
  active = list(
    #' @field values (read-only) Retrieve the coordinate values as timestamps.
    values = function(value) {
      if (missing(value))
        private$.time$format()
    },

    #' @field time (read-only) The `CFTime` instance managing the coordinates.
    time = function(value) {
      if (missing(value))
        private$.time
    },

    #' @field offsets (read-only) Retrieve the numeric offsets of the time
    #'   coordinate values.
    offsets = function(value) {
      if (missing(value)) {
        private$.time$offsets
      }
    }
  )
)

# ========== CoordinatesOrdinal ================================================

#' Ordinal coordinate values
#'
#' @description This class implements ordinal values. Ordinal values are
#'   typically assigned to axes that have no other coordinates assigned to them.
#'   By default, ordinal values start at 0 (as per the Zarr specification) but
#'   after subsetting or other forms of selection the value may be different.
#' @docType class
CoordinatesOrdinal <- R6::R6Class('CoordinatesOrdinal',
  inherit = CoordinatesPacked,
  private = list(
    .bottom = 0L # The lowest index value
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param name Character string. Name of the coordinate set.
    #' @param direction Character string. Direction of the coordinates. Must be
    #'   one from a set of values.
    #' @param length Integer value giving the number of elements in this
    #'   instance.
    #' @param low Optional. Integer value giving the lowest index value in this
    #'   instance. When omitted, defaults to 0L.
    #' @return An instance of this class.
    initialize = function(name, direction, length, low = 0L) {
      if (!is.integer(length) || length(length) != 1L || length < 1L)
        stop('Argument `length` must be a positive integer value', call. = FALSE)

      super$initialize(name, direction, unit = '-', values = c(low, 1L), length)
      private$.bottom = low
    },

    #' @description Return an exact copy of these coordinates.
    #' @return A new instance of `CoordinatesOrdinal`.
    copy = function() {
      CoordinatesOrdinal$new(private$.name, private$.direction, private$.length, private$.bottom)
    },

    #' @description Return a subset of the coordinate values.
    #' @param rng The range of indices whose values from these coordinate values
    #'   to include in the result.
    #' @return A new `CoordinatesOrdinal` instance covering the indicated
    #'   range of values.
    subset = function(rng) {
      rng <- range(rng)
      CoordinatesOrdinal$new(rng[2L] - rng[1L] + 1L, rng[1L])
    }
  )
)

# =============== Helper functions =============================================

#' Analyse the `storage.mode` of the values to determine its type. For numeric
#' and integer types the values are assessed to determine if they are regular.
#'
#' This function cannot deal with time coordinate values.
#'
#' @param values The values to process, a vector.
#'
#' @returns A `list` with elements "mode" and "values" and optionally "length"
#'   if the values are regular. If the `storage.mode` is not supported, `NULL`
#'   is returned.
#' @noRd
.make_coordinate_values <- function(values) {
  switch(storage.mode(values),
    'integer' = {
      if ((len <- length(values)) > 2L) {
        diff <- diff(values)
        if (all(diff[1L], diff))
          return(list(mode = 'integer', values = c(values[1L], diff[1L]), length = len))
      }
      return(list(mode = 'integer', values = values))
    },
    'double' = {
      if ((len <- length(values)) > 2L) {
        diff <- diff(values)
        if (all(.near(diff[1L], diff)))
          return(list(mode = 'double', values = c(values[1L], diff[1L]), length = len))
      }
      return(list(mode = 'double', values = values))
    },
    'character' = return(list(mode = 'character', values = values)),
    NULL
  )
}
