#' Coordinate system axis
#'
#' @description This class implements the coordinate system axis class. The axis
#'   is always associated with a coordinate system.
#'
#'   In an extension over the OGC standard, an axis can have multiple sets of
#'   coordinates. The unit-of-measure is associated with the coordinates and
#'   thus not with the axis.
#'
#' @docType class
CoordinateSystemAxis <- R6::R6Class('CoordinateSystemAxis',
  inherit = IdentifiedObject,
  private = list(
    # OGC. Abbreviation of the axis.
    .abbreviation = NA_character_,

    # Coordinates of the axis. There can be multiple coordinate sets per axis.
    # The .active_coordinates field points to the current set, an instance of (a
    # descendant class of) Coordinates.
    .coordinates = list(),
    .active_coordinates = NULL
  ),
  public = list(
    #' @description Create a new axis instance for use in a coordinate system.
    #' @param name Character string. Name of the axis.
    #' @param abbreviation Character string. Abbreviation of the axis.
    #' @param coordinates A list with one or more named instances of the
    #'   Coordinates class, or any of its descendants. The first element in the
    #'   list will be made active.
    #' @return An instance of this class or an error.
    initialize = function(name, abbreviation, coordinates) {
      super$initialize(name)

      if (is.character(abbreviation) && length(abbreviation) == 1L)
        private$.abbreviation <- abbreviation
      else
        stop('Axis abbreviation must be a character string', call. = FALSE)

      if (!is.list(coordinates))
        coordinates <- list(coordinates)
      if (length(coordinates)) {
        private$.coordinates <- coordinates
        private$.active_coordinates <- private$.coordinates[[1L]]
      } else
        stop('Axis coordinates must be supplied as a list of one or more Coordinates instances', call. = FALSE)
    },

    #' @description Print a summary of the coordinate system axis to the
    #'   console.
    #' @param ... Ignored.
    #' @return Self, invisible.
    print = function(...) {
      cat('<Axis> ', private$.name, '\n', sep = '')
      cat('Abbreviation :', if (nzchar(private$.abbreviation)) private$.abbreviation else '-', '\n')
      cat('Length       :', private$.active_coordinates$length, '\n')
      cat('Coordinates  :\n')
      coords <- do.call(rbind, lapply(private$.coordinates, function(crd) crd$brief()))
      if (nrow(coords) == 1L) coords$name <- NULL
      print(.slim.data.frame(coords, ...), right = FALSE, row.names = FALSE)
      invisible(self)
    },

    #' @description Some details of the axis.
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      vals <- private$.active_coordinates$range
      vals <- paste0('[', vals[1L], ' ... ', vals[2L], ']', sep = '')

      unit <- private$.active_coordinates$getUnit()
      if (!nzchar(unit)) unit <- '-'

      data.frame(abbr = private$.abbreviation,
                 direction = private$.active_coordinates$getDirection(),
                 length = private$.active_coordinates$length,
                 values = vals,
                 unit = unit)
    },

    #' @description Return an exact copy of this axis.
    #' @return A new instance of `CoordinateSystemAxis`.
    copy = function() {
      coordinates <- lapply(private$.coordinates, function(crd) crd$copy())
      CoordinateSystemAxis$new(self$name, self$abbreviation, coordinates)
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #' @param name The name for the new axis. If an empty string is passed
    #'   (default), will use the name of this axis.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis. If the value of the argument is `NULL`, return a
    #'   copy of the axis.
    #' @return A new `CoordinateSystemAxis` instance covering the indicated
    #'   range of indices. If the value of the argument `rng` is `NULL`, return
    #'   a copy of this axis as the new axis.
    subset = function(name = '', rng = NULL) {
      if (is.null(rng))
        self$copy()
      else {
        if (!nzchar(name)) name <- self$name
        coordinates <- lapply(private$.coordinates, function(crd) crd$subset(rng))
        CoordinateSystemAxis$new(name, private$.abbreviation, coordinates)
      }
    },

    #' @description Given a range of domain coordinate values, returns the
    #'   indices into this axis that fall within the supplied range. If the axis
    #'   has bounds, any coordinate whose boundary values fall entirely or
    #'   partially within the supplied range will be included in the result.
    #' @param rng A numeric vector whose extreme values indicate the indices of
    #'   coordinates to return.
    #' @return An integer vector of length 2 with the lower and higher indices
    #'   into the axis that fall within the range of coordinates in argument
    #'   `rng`. Returns `NULL` if no (boundary) values of the axis fall within
    #'   the range of coordinates.
    slice = function(rng) {
      private$.active_coordinates$slice(rng)
    },

    #' @description Retrieve the abbreviation of the axis. This method is
    #'   mandatory in the OGC standard.
    #' @return Character string with the abbreviation of the axis.
    getAbbreviation = function() {
      private$.abbreviation
    },

    #' @description Retrieve the direction of the axis. This method is
    #'   mandatory in the OGC standard.
    #' @return Character string with the direction of the axis.
    getDirection = function() {
      private$.direction
    },

    #' @description Retrieve the unit of measure of the axis. This method is
    #'   mandatory in the OGC standard.
    #' @return Character string with the unit of measure of the axis.
    getUnit = function() {
      private$.unit
    },

    #' @description Retrieve the minimum coordinate value of this axis, in units
    #' of the unit of measure of the axis.
    #' @return Negative infinity.
    getMinimumValue = function() {
      -Inf
    },

    #' @description Retrieve the maximum coordinate value of this axis, in units
    #' of the unit of measure of the axis.
    #' @return Positive infinity.
    getMaximumValue = function() {
      +Inf
    },

    #' @description Retrieve the minimum coordinate value of this axis, in units
    #' of the unit of measure of the axis.
    #' @return The character string 'EXACT'.
    getRangeMeaning = function() {
      'EXACT'
    }
  ),
  active = list(
    #' @field abbreviation Set or retrieve the abbreviation of the axis.
    abbreviation = function(value) {
      if (missing(value))
        private$.abbreviation
      else if (is.character(value))
        private$.abbreviation <- value[1L]
    },

    #' @field coordinates (read-only) Retrieve the currently active coordinates.
    coordinates = function(value) {
      if (missing(value))
        private$.active_coordinates
    },

    #' @field length (read-only) Retrieve the length of the axis.
    length = function(value) {
      if (missing(value)) {
        if (is.null(private$.active_coordinates))
          0L
        else
          private$.active_coordinates$values_object$length
      }
    }
  )
)
