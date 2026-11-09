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
    # List of CoordinateSystemAxis
    .axes = list(),

    # Top-level CRS identifier for the entire CS, always assumed to be compound.
    .crs_id = '',

    # Geolocation arrays for the X and Y axes, a `zarr_convention_geolocation`
    # instance, or NULL.
    .geolocation = NULL,

    # Geolocation data to be written, keyed by "geodetic" and/or "planar".
    # Each element is a list with elements `x` and `y` (each a list with
    # `name`, `values` and `attributes`), `dims` and `id`.
    .geolocation_data = list(),

    # The names of the X and Y axes, or an error when the coordinate system
    # does not have exactly one of each.
    xy_axes = function() {
      abbr <- vapply(private$.axes, function(a) a$abbreviation, character(1L))
      if (sum(abbr == 'X') != 1L || sum(abbr == 'Y') != 1L)
        stop('Geolocation requires exactly one "X" axis and one "Y" axis in the coordinate system', call. = FALSE)
      names(private$.axes)[c(which(abbr == 'X'), which(abbr == 'Y'))]
    }
  ),
  public = list(
    #' @description Create a new coordinate system. The CS must have at least
    #'   one axis.
    #' @param name Character string. Name of the coordinate system.
    #' @param axes A `list` of instances of [CoordinateSystemAxis]. There must
    #'   be at least one axis in the list. The elements in the list must be in
    #'   the order of the dimensions of the array that this coordinate system
    #'   is used for; in other words, the first axis corresponds to the first
    #'   array dimension, etc. - scalar axes (those not associated with an array
    #'   dimension) are listed after the axes associated with a dimension.
    #' @param crs Optional, character string with the description of the CRS of
    #'   this coordinate system, always assumed to be compound.
    #' @param attributes Optional. A `list` of attributes of the coordinate
    #'   system.
    #' @return An instance of `CoordinateSystem` or an error.
    initialize = function(name, axes, crs = '', attributes = list()) {
      super$initialize(name, attributes)

      if (is.list(axes) && length(axes) >= 1L &&
          all(sapply(axes, inherits, "CoordinateSystemAxis")))
        private$.axes <- axes
      else
        stop("Argument `axes` is not a list containing `CoordinateSystemAxis` instances.", call. = FALSE)

      if (is.null(crs)) crs <- ''
      private$.crs_id <- crs
    },

    #' @description Print a summary of the coordinate system to the console.
    #' @param ... Ignored.
    #' @return Self, invisibly.
    print = function(...) {
      thm <- zarr::zarr_options('zarr_theme')
      if (is.list(thm)) {
        this_div <- cli::cli_div(theme = thm)
        cli::cli_h1('Coordinate system')
        cli::cli_end(this_div)
      } else
        cat("<Coordinate System>\n")
      self$print_axes(...)
      self$print_attributes()
      invisible(self)
    },

    #' @description Prints details of the axes of the coordinate system to the
    #'   console. Used internally.
    #' @param ... Arguments passed on to `data.frame` printing.
    print_axes = function(...) {
      axes <- do.call(rbind, lapply(private$.axes, function(a) a$brief()))
      thm <- zarr::zarr_options('zarr_theme')
      if (is.list(thm)) {
        this_div <- cli::cli_div(theme = thm)
        lines <- capture.output(print(.slim.data.frame(axes, ...), right = FALSE, row.names = FALSE))
        lines[1L]  <- cli::format_inline('{.label {lines[1L]}}')
        lines[-1L] <- vapply(lines[-1L], function(l) cli::format_inline('{.text {l}}'), '', USE.NAMES = FALSE)
        cli::cli_verbatim(lines)
        cli::cli_end(this_div)
      } else
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
    },

    #' @description Set geolocation data for the coordinate system, giving
    #'   coordinates in a geodetic or planar CRS for every element of the "X"
    #'   and "Y" axes. The data is written to the Zarr store with
    #'   `geozarr_array$write_geolocation()`, after which it is removed from
    #'   memory and the `geolocation` field references the written arrays.
    #'   Any geolocation of the same `kind` is replaced.
    #' @details An error is thrown when the coordinate system does not have
    #'   exactly one axis with abbreviation "X" and one with abbreviation "Y";
    #'   when `dims` does not include the names of these two axes or names an
    #'   axis that is not in the coordinate system; when the dimensions of `x`
    #'   or `y` do not match the lengths of the axes in `dims`; when only one
    #'   of `x_bounds` and `y_bounds` is given; when `bounds_dims` is not
    #'   `dims` plus one other name that is not an axis of the coordinate
    #'   system; when the dimensions of `x_bounds` or `y_bounds` do not match
    #'   the lengths of the axes in `bounds_dims`, with length 4 for the vertex
    #'   dimension; or when the other arguments are malformed.
    #' @param kind Character string, either "geodetic" or "planar".
    #' @param x,y Numeric arrays with the coordinates of the X and Y axes in the
    #'   target CRS, i.e. longitude and latitude for "geodetic".
    #' @param dims Character vector with the names of the axes of the
    #'   coordinate system that the dimensions of `x` and `y` correspond to, in
    #'   order.
    #' @param names Character vector of two values with the names of the Zarr
    #'   arrays to write `x` and `y` to.
    #' @param id Optional. An instance of [zarr_conv_proj] identifying the
    #'   target CRS.
    #' @param attributes Optional. A `list` of two named lists (not data
    #'   frames) with the
    #'   attributes to write with the arrays for `x` and `y`, such as "units"
    #'   and "standard_name".
    #' @param x_bounds,y_bounds Optional. Numeric arrays with the four vertices
    #'   of each cell of `x` and `y`, in the order of the geolocation
    #'   convention. Must supply both or neither.
    #' @param bounds_dims Character vector with the names of the dimensions of
    #'   `x_bounds` and `y_bounds`, in order: the names in `dims` plus the name
    #'   of the vertex dimension, in any order. Required with `x_bounds` and
    #'   `y_bounds`.
    #' @param bounds_names Optional character vector of two values with the
    #'   names of the Zarr arrays to write `x_bounds` and `y_bounds` to.
    #'   Defaults to `names` with "_bnds" appended.
    #' @return Self, invisibly.
    set_geolocation = function(kind, x, y, dims, names, id = NULL, attributes = list(list(), list()),
                               x_bounds = NULL, y_bounds = NULL, bounds_dims = NULL, bounds_names = NULL) {
      kind <- match.arg(kind, c('geodetic', 'planar'))
      xy <- private$xy_axes()
      if (!is.character(names) || length(names) != 2L || any(!nzchar(names)) || names[1L] == names[2L])
        stop('Argument `names` must be two different non-empty character strings', call. = FALSE)
      if (!is.character(dims) || anyDuplicated(dims) || !all(dims %in% names(private$.axes)))
        stop('Argument `dims` must name axes of the coordinate system', call. = FALSE)
      if (!all(xy %in% dims))
        stop('Argument `dims` must include the X and Y axes "', xy[1L], '" and "', xy[2L], '"', call. = FALSE)
      lengths <- vapply(private$.axes[dims], function(a) a$length, integer(1L), USE.NAMES = FALSE)
      for (v in list(x, y))
        if (!is.numeric(v) || !identical(as.integer(dim(v) %||% length(v)), lengths))
          stop('Arguments `x` and `y` must be numeric arrays matching the lengths of the axes in `dims`', call. = FALSE)
      if (!is.null(id) && !inherits(id, 'zarr_conv_proj'))
        stop('Argument `id` must be an instance of `zarr_conv_proj`', call. = FALSE)
      named_list <- function(a) is.list(a) && !is.data.frame(a) && (!length(a) || all(nzchar(names(a) %||% '')))
      if (!is.list(attributes) || length(attributes) != 2L || !all(vapply(attributes, named_list, logical(1L))))
        stop('Argument `attributes` must be a list of two named lists', call. = FALSE)

      data <- list(x = list(name = names[1L], values = x, attributes = attributes[[1L]]),
                   y = list(name = names[2L], values = y, attributes = attributes[[2L]]),
                   dims = dims, id = id)

      if (is.null(x_bounds) != is.null(y_bounds))
        stop('Arguments `x_bounds` and `y_bounds` must both be given, or neither', call. = FALSE)
      if (!is.null(x_bounds)) {
        vertex <- setdiff(bounds_dims, dims)
        if (!is.character(bounds_dims) || anyDuplicated(bounds_dims) || !all(dims %in% bounds_dims) ||
            length(vertex) != 1L || vertex %in% names(private$.axes))
          stop('Argument `bounds_dims` must hold the names in `dims` and the name of a vertex dimension', call. = FALSE)
        bounds_names <- bounds_names %||% paste0(names, '_bnds')
        if (!is.character(bounds_names) || length(bounds_names) != 2L || any(!nzchar(bounds_names)) ||
            anyDuplicated(c(names, bounds_names)))
          stop('Argument `bounds_names` must be two different non-empty character strings, other than `names`', call. = FALSE)
        blengths <- ifelse(bounds_dims == vertex, 4L, lengths[match(bounds_dims, dims)])
        for (v in list(x_bounds, y_bounds))
          if (!is.numeric(v) || !identical(as.integer(dim(v)), blengths))
            stop('Arguments `x_bounds` and `y_bounds` must be numeric arrays matching the lengths of the axes in `bounds_dims`, with 4 vertices', call. = FALSE)
        data$x$bounds <- list(name = bounds_names[1L], values = x_bounds)
        data$y$bounds <- list(name = bounds_names[2L], values = y_bounds)
        data$bounds_dims <- bounds_dims
      }

      private$.geolocation_data[[kind]] <- data
      if (!is.null(private$.geolocation)) {
        refs <- private$.geolocation$as_list()
        refs[[kind]] <- NULL
        private$.geolocation <- if (length(refs)) zarr_convention_geolocation$new(refs)
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field axes (read-only) Retrieve the list of axes that make up this
    #'   coordinate system.
    axes = function(value) {
      if (missing(value))
        private$.axes
    },

    #' @field crs Set or retreive the CRS identifier of the coordinate system.
    crs = function(value) {
      if (missing(value))
        private$.crs_id
      else if (is.character(value))
        private$.crs_id <- value[1L]
    },

    #' @field geolocation Set or retrieve the geolocation arrays of the
    #'   coordinate system, an instance of [zarr_convention_geolocation], or
    #'   `NULL` when there are none. The `x` and `y` arrays of the geolocation
    #'   correspond to the axes of the coordinate system with abbreviation "X"
    #'   and "Y", respectively. Set to `NULL` to remove the geolocation.
    #'   Setting the field removes any geolocation data of the same kinds that
    #'   was set with `set_geolocation()`.
    #'
    #'   An error is thrown when setting a value that is not a
    #'   `zarr_convention_geolocation` instance with geolocation arrays set, or
    #'   when the coordinate system does not have exactly one axis with
    #'   abbreviation "X" and one with abbreviation "Y".
    geolocation = function(value) {
      if (missing(value))
        private$.geolocation
      else if (is.null(value))
        private$.geolocation <- NULL
      else {
        if (!inherits(value, 'zarr_convention_geolocation'))
          stop('Geolocation must be an instance of `zarr_convention_geolocation`', call. = FALSE)
        kinds <- names(value$as_list()) # Errors when no arrays are set
        private$xy_axes()
        private$.geolocation <- value
        private$.geolocation_data[kinds] <- NULL
      }
    },

    #' @field geolocation_data (read-only) The geolocation data set with
    #'   `set_geolocation()` that has not yet been written to a Zarr store, a
    #'   `list` keyed by "geodetic" and/or "planar". Empty when there is none.
    geolocation_data = function(value) {
      if (missing(value))
        private$.geolocation_data
    }
  )
)
