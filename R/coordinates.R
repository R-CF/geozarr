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

#' Coordinates
#'
#' @description This class implements the coordinates class. The coordinates are
#'   always associated with a coordinate system axis. This class is not part of
#'   the OGC standard but rather an extension to allow an axis to have multiple
#'   sets of coordinates.
#'
#'   By default the class has numeric coordinate values. For integer values
#'   (e.g. for an ordinal axis), string values, or values that require special
#'   handling (e.g. time coordinates) descendant classes should be used.
#'
#'   Coordinate values are represented as a sequence of two values whenever
#'   possible: the coordinate value of the first element and the constant
#'   spacing between successive values. If the separation between values is not
#'   constant, a full vector with coordinate values is stored.
#'
#'   A similar arrangement is used for the boundary values that define the
#'   finite extent of each element in the coordinates. When regular, the bounds
#'   indicate the amount below and above the coordinate value, respectively,
#'   that define the extent of each element. Otherwise, it is a matrix with
#'   explicit boundary values below (row 1) and above (row 2) each coordinate
#'   value (in the columns).
#' @docType class
Coordinates <- R6::R6Class('Coordinates',
  cloneable = FALSE,
  private = list(
    .name = NA_character_,
    .direction = NA_character_,
    .unit = NA_character_,

    .values = NULL,
    .bounds = NULL,

    # This method return a vector of the extreme values of the coordinates. By
    # default the .values are taken. Descendant classes should override this as
    # necessary.
    coordinate_range = function() {
      private$.values$range()
    }
  ),
  public = list(
    #' @description Create a new coordinates instance for use with a coordinate
    #'   system axis.
    #' @param name Character string. Name of the coordinate set.
    #' @param direction Character string. Direction of the coordinates. Must be
    #'   one from a set of values.
    #' @param unit Character string. Unit of measure of the coordinates.
    #' @param values An instance of a descendant class of CoordinateValues.
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
    initialize = function(name, direction, unit, values, bounds) {
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

      private$.values <- values
      if (!missing(bounds) && !is.null(bounds))
        private$.bounds <- bounds
    },

    #' @description Print a summary of the coordinates to the console.
    #' @param ... Ignored.
    #' @return Self, invisible.
    print = function(...) {
      cat('<Coordinates> ', private$.name, '\n', sep = '')
      cat('Length    :', private$.values$length, '\n')
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

    #' @field values (read-only) Retrieve the coordinate values.
    values = function(value) {
      if (missing(value))
        private$.values
    },

    #' @field range (read-only) Retrieve the extreme values of the coordinates.
    range = function(value) {
      private$coordinate_range()
    },

    #' @field length (read-only) The number of elements in this instance once
    #'   the values are unpacked.
    length = function(value) {
      if (missing(value))
        private$.values$length
    }
  )
)

#' Create a CoordinateValues object from the arguments. This will look at the
#' `storage.mode` of the values to determine its type. For numeric and integer
#' types the values are assessed to determine if they are regular.
#'
#' This function cannot deal with time coordinate values.
#'
#' @param values The values to process, a vector.
#'
#' @returns An instance of a class descending from `CoordinateValues`. If the
#'   `storage.mode` is not supported, `NULL` is returned.
#' @noRd
.make_coordinate_values <- function(values) {
  switch(storage.mode(values),
    # 'logical'   = ,
    'integer' = {
      if ((len <- length(values)) > 2L) {
        if (all(values[1L], values))
          return(CoordinateValuesIntegerPacked$new(c(values[1L], values[2L] - values[1L]), len))
      }
      return(CoordinateValuesInteger$new(values))
    },
    'double' = {
      if ((len <- length(values)) > 2L) {
        diff <- diff(values)
        if (all(.near(diff[1L], diff)))
          return(CoordinateValuesNumericPacked$new(c(values[1L], diff[1L]), len))
      }
      return(CoordinateValuesNumeric$new(values))
    },
    'character' = return(CoordinateValuesCharacter$new(values)),
    NULL
  )
}
