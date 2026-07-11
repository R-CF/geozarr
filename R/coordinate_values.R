#' Coordinate values
#'
#' @description This class implements the CoordinateValues class. The
#'   Coordinates class has an instance of a descendant of this class to
#'   represent its coordinate values. This "virtual" base class should not be
#'   instantiated directly; instead, use one of the descendant classes. These
#'   descendant classes are specific to a data type (numeric, integer, logical,
#'   character, time, parametric) and a storage mode (regular, explicit).
#' @docType class
CoordinateValues <- R6::R6Class('CoordinateValues',
  private = list(
    # The values of this instance. May be packed or unpacked. May need further
    # processing, such as with time coordinates or parametric formulations.
    .values = NULL,

    # The number of values in the instance of this class.
    .length = 0L,

    # Get the values. Packed descendant classes must override this method.
    unpacked = function() {
      private$.values
    }
  ),
  public = list(
    #' @description Create an instance of this class. Not to be called directly,
    #' use a descendant class instead.
    #' @param length The number of elements in this instance.
    #' @return An instance of this class.
    initialize = function(length) {
      private$.length <- length
    },

    #' @description Print a summary of the coordinate values to the console.
    #' @param ... Ignored.
    #' @return Self, invisible.
    print = function(...) {
      cat('<Coordinate values>\n')
      len <- private$.length
      cat('Length    :', len, '\n')
      vals <- private$unpacked()
      cat('Values    : [', vals[1L], ' ... ', vals[len], '] ', sep = '')
      invisible(self)
    },

    #' @description Retrieve the range of the values, as a vector of two values.
    #'   Packed classes must override this method.
    #' @return A vector of two extreme values.
    range = function() {
      range(private$.values)
    },

    #' @description Given a range of coordinate values, returns the indices of
    #'   the coordinate values that fall within the supplied range.
    #' @param rng A numeric vector whose extreme values indicate the indices of
    #'   coordinate values to return.
    #' @return An integer vector of length 2 with the lower and higher indices
    #'   that fall within the range of coordinate values in argument `rng`.
    #'   Returns `NULL` if no coordinate values fall within the range of
    #'   argument `rng`.
    slice = function(rng) {
      vals <- self$values
      if (is.null(vals)) return(NULL)
      rng <- range(rng)
      idx <- which(vals >= rng[1L] & vals <= rng[2L])
      if (!length(idx)) NULL else as.integer(range(idx))
    }
  ),
  active = list(
    #' @field values (read-only) The values of this instance. Values are always
    #'   returned as a full set.
    values = function(value) {
      if (missing(value))
        private$unpacked()
    },

    #' @field raw (read-only) The raw values of this instance, may be a full set
    #'   or two packed values.
    raw = function(value) {
      if (missing(value))
        private$.values
    },

    #' @field length (read-only) The number of elements in this instance once
    #'   the values are unpacked.
    length = function(value) {
      if (missing(value))
        private$.length
    }
  )
)

#' Ordinal coordinate values
#'
#' @description This class implements ordinal values. Ordinal values are
#'   typically assigned to axes that have no other coordinates assigned to them.
#'   By default, ordinal values start at 0 (as per the Zarr specification) but
#'   after subsetting or other forms of selection the value may be different.
#' @docType class
CoordinateValuesOrdinal <- R6::R6Class('CoordinateValuesOrdinal',
  inherit = CoordinateValues,
  private = list(
    .bottom = 0L # The lowest index value
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param length Integer value giving the number of elements in this
    #'   instance.
    #' @param low Optional. Integer value giving the lowest index value in this
    #'   instance. When omitted, defaults to 0L.
    #' @return An instance of this class.
    initialize = function(length, low = 0L) {
      if (!is.integer(length) || length(length) != 1L || length < 1L)
        stop('Argument `length` must be a positive integer value', call. = FALSE)

      super$initialize(length)
      private$.bottom = low
    },

    #' @description Return a subset of the coordinate values.
    #' @param rng The range of indices whose values from these coordinate values
    #'   to include in the result.
    #' @return A new `CoordinateValuesOrdinal` instance covering the indicated
    #'   range of values.
    subset = function(rng) {
      rng <- range(rng)
      CoordinateValuesOrdinal$new(rng[2L] - rng[1L] + 1L, rng[1L])
    },

    #' @description Retrieve the range of the values, as a vector of two values.
    #' @return A vector of two extreme values.
    range = function() {
      c(private$.bottom, private$.bottom + private$.length - 1L)
    }
  ),
  active = list(
    #' @field values (read-only) The values of this instance.
    values = function(value) {
      if (missing(value))
        private$.bottom + seq(private$.length) - 1L
    },

    #' @field raw (read-only) The values of this instance.
    raw = function(value) {
      if (missing(value))
        self$values
    }
  )
)

#' Integer coordinate values
#'
#' @description This class implements integer coordinate values. Integer
#'   coordinate values are typically used for categorical data.
#' @docType class
CoordinateValuesInteger <- R6::R6Class('CoordinateValuesInteger',
  inherit = CoordinateValues,
  public = list(
    #' @description Create an instance of this class.
    #' @param values The values in this class instance, an integer vector.
    #' @return An instance of this class.
    initialize = function(values) {
      if (!is.integer(values) || !length(values) || !.monotonicity(values))
        stop('Argument `values` must be a vector of monotonous integer values', call. = FALSE)

      super$initialize(length(values))
      private$.values <- values
    },

    #' @description Return a subset of the coordinate values.
    #' @param rng The range of indices whose values from these coordinate values
    #'   to include in the result.
    #' @return A new `CoordinateValuesInteger` instance covering the indicated
    #'   range of values.
    subset = function(rng) {
      rng <- range(rng)
      CoordinateValuesInteger$new(private$.values[rng[1L]:rng[2L]])
    }
  )
)

#' Packed integer coordinate values
#'
#' @description This class implements packed integer coordinate values. Integer
#'   coordinate values are typically used for categorical data.
#' @docType class
CoordinateValuesIntegerPacked <- R6::R6Class('CoordinateValuesIntegerPacked',
  inherit = CoordinateValues,
  private = list(
    # Override the inherited function to compute full set of values from the
    # packed values.
    unpacked = function() {
      seq(from = private$.values[1L], by = private$.values[2L], length = private$.length)
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param values The values in this class instance, two numeric values. The
    #'   first value represents the first element along the set of values, the
    #'   second value is the increment to compute subsequent values, possibly
    #'   negative. The second value may not be 0.
    #' @param length Integer value indicating the number of elements in the
    #'   unpacked data.
    #' @return An instance of this class.
    initialize = function(values, length) {
      if (!is.integer(values) || length(values) != 2L)
        stop('Argument `values` must be an integer vector of length 2', call. = FALSE)
      if (values[2L] == 0L)
        stop('Argument `values` must have a non-zero second value', call. = FALSE)

      super$initialize(length)
      private$.values <- values
    },

    #' @description Retrieve the range of the values, as a vector of two values.
    #' @return A vector of two extreme values.
    range = function() {
      c(private$.values[1L], private$.values[1L] + (private$.length - 1L) * private$.values[2L])
    },

    #' @description Return a subset of the coordinate values.
    #' @param rng The range of indices whose values from these coordinate values
    #'   to include in the result, relative to the unpacked values.
    #' @return A new `CoordinateValuesIntegerPacked` instance covering the
    #'   indicated range of values.
    subset = function(rng) {
      rng <- range(rng)
      CoordinateValuesIntegerPacked$new(c(private$.values[1L] + private$.values[2L] * (rng[1L] - 1L), private$.values[2L]),
                                        rng[2L] - rng[1L] + 1L)
    }
  )
)

#' Numeric coordinate values
#'
#' @description This class implements numeric coordinate values. Numeric
#'   coordinate values are very commonly used for axes that have a continuous
#'   physical property, such as longitude or depth.
#' @docType class
CoordinateValuesNumeric <- R6::R6Class('CoordinateValuesNumeric',
  inherit = CoordinateValues,
  public = list(
    #' @description Create an instance of this class.
    #' @param values The values in this class instance.
    #' @return An instance of this class.
    initialize = function(values) {
      if (!is.numeric(values) || !length(values) || !.monotonicity(values))
        stop('Argument `values` must be a vector of monotonous numeric values', call. = FALSE)

      super$initialize(length(values))
      private$.values <- values
    },

    #' @description Return a subset of the coordinate values.
    #' @param rng The range of indices whose values from these coordinate values
    #'   to include in the result.
    #' @return A new `CoordinateValuesNumeric` instance covering the indicated
    #'   range of values.
    subset = function(rng) {
      rng <- range(rng)
      CoordinateValuesNumeric$new(private$.values[rng[1L]:rng[2L]])
    }
  )
)

#' Packed numeric coordinate values
#'
#' @description This class implements packed numeric coordinate values. Numeric
#'   coordinate values are very commonly used for axes that have a continuous
#'   physical property, such as longitude or depth.
#' @docType class
CoordinateValuesNumericPacked <- R6::R6Class('CoordinateValuesNumericPacked',
  inherit = CoordinateValues,
  private = list(
    # Override the inherited function to compute full set of values from the
    # packed values.
    unpacked = function() {
      seq(from = private$.values[1L], by = private$.values[2L], length = private$.length)
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param values The values in this class instance, two numeric values. The
    #'   first value represents the first element along the set of values, the
    #'   second value is the increment to compute subsequent values, possibly
    #'   negative. The second value may not be 0.
    #' @param length Integer value indicating the number of elements in the
    #'   unpacked data.
    #' @return An instance of this class.
    initialize = function(values, length) {
      if (!is.numeric(values) || length(values) != 2L)
        stop('Argument `values` must be a numeric vector of length 2', call. = FALSE)
      if (.near(values[2L], 0))
        stop('Argument `values` must have a non-zero second value', call. = FALSE)

      super$initialize(length)
      private$.values <- values
    },

    #' @description Retrieve the range of the values, as a vector of two values.
    #' @return A vector of two extreme values.
    range = function() {
      c(private$.values[1L], private$.values[1L] + (private$.length - 1L) * private$.values[2L])
    },

    #' @description Return a subset of the coordinate values.
    #' @param rng The range of indices whose values from these coordinate values
    #'   to include in the result, relative to the unpacked values.
    #' @return A new `CoordinateValuesNumericPacked` instance covering the
    #'   indicated range of values.
    subset = function(rng) {
      rng <- range(rng)
      CoordinateValuesNumericPacked$new(c(private$.values[1L] + private$.values[2L] * (rng[1L] - 1L), private$.values[2L]),
                                        rng[2L] - rng[1L] + 1L)
    }
  )
)

#' String-valued coordinate values
#'
#' @description This class implements string-valued coordinate values.
#'   String-valued coordinate values are very commonly used for categorical
#'   data.
#' @docType class
CoordinateValuesString <- R6::R6Class('CoordinateValuesString',
  inherit = CoordinateValues,
  public = list(
    #' @description Create an instance of this class.
    #' @param values The values in this class instance, a character vector.
    #' @return An instance of this class.
    initialize = function(values) {
      if (!is.character(values) || !length(values))
        stop('Argument `values` must be a character vector', call. = FALSE)

      super$initialize(length(values))
      private$.values <- values
    },

    #' @description Return a subset of the coordinate values.
    #' @param rng The range of indices whose values from these coordinate values
    #'   to include in the result.
    #' @return A new `CoordinateValuesString` instance covering the indicated
    #'   range of values.
    subset = function(rng) {
      rng <- range(rng)
      CoordinateValuesString$new(private$.values[rng[1L]:rng[2L]])
    },

    #' @description Given a range of coordinate values, returns the indices that
    #'   fall within the supplied range.
    #' @param rng A character vector whose extreme (alphabetic) values indicate
    #'   the indices of coordinate values to return.
    #' @return An integer vector of length 2 with the lower and higher indices
    #'   that fall within the range of coordinate values in argument `rng`.
    #'   Returns `NULL` if no values of the axis fall within the range of
    #'   coordinates.
    slice = function(rng) {
      res <- range(match(rng, self$values, nomatch = 0L), na.rm = TRUE)
      if (all(res == 0L)) NULL
      else if (res[1L] == 0L) c(res[2L], res[2L])
      else if (res[2L] == 0L) c(res[1L], res[1L])
      else res
    }
  )
)
