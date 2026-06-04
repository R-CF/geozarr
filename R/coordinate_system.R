#' Coordinate system
#'
#' @description This class implements the coordinate system class. This class
#' definition implements the same class from the OGC geoAPI Java package,
#' extended with specific code for this package.
#'
#' @docType class
#' @export
CoordinateSystem <- R6::R6Class("CoordinateSystem",
  inherit = IdentifiedObject,
  private = list(
    .axes = list()   # list of CoordinateSystemAxis
  ),
  public = list(
    #' @description Create a new coordinate system. The CS must have at least
    #' one axis.
    #' @param name Character string. Name of the coordinate system.
    #' @param axes A `list` of instances of [CoordinateSystemAxis]. There must
    #' be at least one axis in the list.
    #' @return An instance of `CoordinateSystem` or an error.
    initialize = function(name, axes) {
      super$initialize(name)
      # FIXME: Must check that the list of axes form a valid CS
      if (is.list(axes) && length(axes) >= 1L &&
          all(sapply(axes, inherits, "CoordinateSystemAxis")))
        private$.axes <- axes
      else
        stop("Argument `axes` is not a list containing `CoordinateSystemAxis` instances.", call. = FALSE)
    },

    #' @description Print a summary of the coordinate system to the console.
    #' @param ... Ignored.
    #' @return Self, invisibly.
    print = function(...) {
      cat("<Coordinate System>\n")
      self$print_axes(...)
      invisible(self)
    },

    #' @description Prints details of the axes of the coordinate system to the
    #'   console. Used internally.
    #' @param ... Arguments passed on to `data.frame` printing.
    print_axes = function(...) {
      axes <- do.call(rbind, lapply(private$.axes, function(a) a$brief()))
      print(.slim.data.frame(axes, ...), right = FALSE, row.names = FALSE)
    },

    #' @description Retrieve the dimension of the coordinate system, the number
    #' of axes that make up the coordinate system.
    #' @return Integer value with the dimension of the coordinate system.
    getDimension = function() {
      length(private$.axes)
    },

    #' @description Retrieve an axis of the coordinate system by its ordinal
    #' number.
    #' @param dimension The dimension whose axis to retrieve. Note that this is
    #' 1-based.
    #' @return The [CoordinateSystemAxis] that forms the `dimension` of the
    #' coordinate system, or an error if the `dimension` argument in deficient.
    getAxis = function(dimension) {
      private$.axes[[dimension]]
    }
  ),
  active = list(
    #' @field axes (read-only) Retrieve the list of axes that make up this
    #'   coordinate system.
    axes = function(value) {
      if (missing(value))
        private$.axes
    }
  )
)
