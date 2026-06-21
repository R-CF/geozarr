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
    #' @param calendar Charatcer string, optional. The calendar to use for the
    #'   time coordinates. Must be one of the calendars supported by the CF
    #'   Metadata Conventions. If not given, defaults to "standard".
    #' @param values An instance of a descendant class of [CoordinateValues].
    #'   They may be numeric, in which case they are taken to be offsets from
    #'   the epoch in the given unit, possibly packed, or character, in which
    #'   case they must be ISO 8601 time stamps.
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
    initialize = function(name, direction, unit, epoch, calendar = 'standard', values, bounds) {
      super$initialize(name, direction, unit, values, bounds)
      private$.time <- try(CFtime::CFTime$new(definition = paste(unit, 'since', epoch),
                                              calendar = calendar,
                                              offsets = values$values), silent = TRUE)
      if (inherits(private$.time, 'try-error'))
        stop('Arguments do not form a valid calendar definition', call. = FALSE)
    }
  ),
  active = list(
    #' @field values (read-only) Retrieve the coordinate values as timestamps.
    values = function(value) {
      if (missing(value))
        private$.time$format()
    }
  )
)
