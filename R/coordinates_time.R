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
