#' GeoZarr "spatial:" convention
#'
#' @description This class implements the GeoZarr "spatial:" convention. In
#'   particular, the following convention is implemented here:
#'
#' ```{r schema, eval = FALSE}
#' {
#'   "schema_url": "https://raw.githubusercontent.com/zarr-conventions/spatial/refs/tags/v1/schema.json",
#'   "spec_url": "https://github.com/zarr-conventions/spatial/blob/v1/README.md",
#'   "uuid": "689b58e2-cf7b-45e0-9fff-9cfc0883d6b4",
#'   "name": "spatial:",
#'   "description": "Spatial coordinate information"
#' }
#' ```
#' @docType class
#' @export
zarr_conv_spatial <- R6::R6Class('zarr_conv_spatial',
  inherit = zarr_convention,
  cloneable = FALSE,
  private = list(
    # Required: Vector of names of spatial dimensions, must be present in the
    # "dimension_names" attribute of arrays.
    .dimensions = character(0),

    # Optional: Coordinates of the outer boundaries of the array. Either 4
    # values for a 2D array `[xmin, ymin, xmax, ymax]`, or 6 for a 3D array
    # `[xmin, ymin, zmin, xmax, ymax, zmax]`.
    .bbox = numeric(0),

    # Optional: Type of transformation. Currently only "affine" is supported.
    .transform_type = 'affine',

    # Conditional, required when `.transform_type == 'affine`: Affine
    # transformation coefficients, six values in order (1) X resolution; (2) 0;
    # (3) X coordinate of UL corner; (4) 0; (5) Y resolution; (6) Y coordinate
    # of UL corner.
    .transform = numeric(0),

    # Optional: Shape of the spatial dimensions
    .shape = numeric(0),

    # Optional: Grid cell registration. "pixel" (the default) means that the
    # coordinates of the UL corner of each grid cell are recorded; "node" means
    # that the coordinates of the center of each grid cell are recorded.
    .registration = 'pixel'
  ),
  public = list(
    #' @description Create a new instance of a "spatial" convention agent.
    #' @return A new instance of a "spatial" convention agent.
    initialize = function() {
      super$initialize(name   = 'spatial:',
                       schema = 'https://raw.githubusercontent.com/zarr-conventions/spatial/refs/tags/v1/schema.json',
                       uuid   = '689b58e2-cf7b-45e0-9fff-9cfc0883d6b4')
      private$.spec <- 'https://github.com/zarr-conventions/spatial/blob/v1/README.md'
      private$.description <- 'Spatial coordinate information'
    },

    #' @description Set the coordinate system for this instance. This method
    #'   sets the affine transform coefficients, as well as the grid cell
    #'   registration and the bounding box.
    #' @param x,y,z Coordinates for the `X`, `Y` and `Z` (or band) axes, as a
    #'   numeric vector of two values: the coordinate of the top-left grid cell
    #'   and the resolution along the axis, respectively. Note that for `y` this
    #'   is typically the largest coordinate value and a negative resolution.
    #'   Coordinates for `x` and `y` must be provided; `z` may be missing or
    #'   `NULL`.
    #' @param shape The length of each axis `x`, `y` and `z`.
    #' @param registration Grid cell registration. "pixel" (the default) means
    #'   that the coordinates in `x`, `y` and `z` are interpreted as the UL
    #'   corner of each grid cell; "node" means that the coordinates are
    #'   interpreted as the center of each grid cell.
    #' @return Self, invisibly.
    set_coordinates = function(x, y, z, shape, registration = 'pixel') {
      if (!is.numeric(x) || !length(x) == 2L || !is.numeric(y) || !length(y) == 2L)
        stop('Must supply `x` and `y` coordinates and resolution.', call. = FALSE)
      if (!is.null(z) && (!is.numeric(z) || !length(z) == 2L))
        stop('When given, must supply `z` coordinates and resolution.', call. = FALSE)
      if (!is.integer(shape) || !length(shape) %in% 2:3)
        stop('`shape` parameter must be an integer vector of length 2 or 3.', call. = FALSE)
      if (!registration %in% c('node', 'pixel'))
        stop('Argument `registration` must be "node" or "pixel"', call. = FALSE)

      private$.transform <- c(x[2L], 0, x[1L], 0, y[2L], y[1L])
      private$.registration <- registration

      private$.bbox <- if (registration == 'pixel')
        c(x[1L], y[1L] + shape[2L] * y[2L], x[1L] + shape[1L] * x[2L], y[1L])
      else {
        halfx <- x[2L] * 0.5
        halfy <- y[2L] * 0.5
        c(x[1L] - halfx, y[1L] + shape[2L] * y[2L] - halfy, x[1L] + shape[1L] * x[2L] - halfx, y[1L] - halfy)
      }
      invisible(self)
    },

    #' @description Write the data of this instance in the attributes of a Zarr
    #'   object.
    #' @param attributes A `list` with Zarr attributes for a group or array. The
    #'   properties will be written at the root level of `attributes`.
    #' @return The updated attributes.
    write = function(attributes) {
      if (!length(private$.dimensions))
        stop('`spatial:dimensions` attribute must be set.', call. = FALSE)
      if (private$.transform_type == 'affine' && !length(private$.transform))
        stop('`spatial:transform` attribute must be set.', call. = FALSE)

      attributes$`spatial:dimensions` <- private$.dimensions
      attributes$`spatial:transform_type` <- private$.transform_type
      attributes$`spatial:transform` <- private$.transform
      if (length(private$.bbox))
        attributes$`spatial:bbox` <- private$.bbox
      if (length(private$.shape))
        attributes$`spatial:shape` <- private$.shape
      attributes$`spatial:registration` <- private$.registration
      attributes
    }
  ),
  active = list(
    #' @field dimensions The "spatial:dimensions" attribute, a character vector
    #'   of dimension names for the X, Y and possible third axes. These names
    #'   must correspond to the names in the "dimension_names" attribute of the
    #'   array that this convention relates to.
    dimensions = function(value) {
      if (missing(value))
        private$.dimensions
      else if (is.character(value) && length(value) %in% 1:3)
        private$.dimensions <- value
      else
        stop('`spatial:dimensions` must be character vector with 1, 2 or 3 dimension names.', call. = FALSE)
    },

    #' @field bbox The "spatial:bbox" attribute, a numeric vector of 4 or 6
    #'   values in order `xmin, ymin, zmin, xmax, ymax, zmax` (with `zmin` and
    #'   `zmax` optional) giving the boundary coordinates of the data array.
    bbox = function(value) {
      if (missing(value))
        private$.bbox
      else if (is.numeric(value) && length(value) == 2L * length(private$.dimensions))
        private$.bbox <- value
      else
        stop('`spatial:bbox` must be a numeric vector with 2 values for each dimension.', call. = FALSE)
    },

    #' @field transform_type (read-only) The "spatial:transform_type" attribute,
    #'   a character string giving the type of coordinate transformation. The
    #'   only valid value (currently) is "affine".
    transform_type = function(value) {
      if (missing(value))
        private$.transform_type
      else
        stop("Can't change the value of `spatial:transform_type`.", call. = FALSE)
    },

    #' @field transform The "spatial:transform" attribute, a numeric vector of 6
    #'   values (1) X resolution; (2) 0; (3) X coordinate of UL corner; (4) 0;
    #'   (5) Y resolution; (6) Y coordinate of UL corner, giving the
    #'   transformation coefficients of the coordinates of the data array.
    transform = function(value) {
      if (missing(value))
        private$.transform
      else if (is.numeric(value) && length(value) %in% c(6L, 12L))
        private$.transform <- value
      else
        stop('`spatial:transform` must be a numeric vector of length 6 (2D) or 12 (3D).', call. = FALSE)
    },

    #' @field shape The "spatial:shape" attribute, an integer vector of length 2
    #'   or 3, giving the length of the X, Y and Z dimensions.
    shape = function(value) {
      if (missing(value))
        private$.shape
      else if (is.integer(value) && length(value) == length(private$.dimensions))
        private$.shape <- value
      else
        stop('`spatial:shape` must be a numeric vector the same length as `spatial:dimensions`.', call. = FALSE)
    },

    #' @field registration The "spatial:registration" attribute, a string with
    #'   value "node" or "pixel". "pixel" (the default) means that the
    #'   coordinates of the UL corner of each grid cell are recorded; "node"
    #'   means that the coordinates of the center of each grid cell are
    #'   recorded.
    registration = function(value) {
      if (missing(value))
        private$.registration
      else if (value %in% c('node', 'pixel'))
        private$.registration <- value
      else
        stop('Bad value for `spatial:registration`.', call. = FALSE)
    }
  )
)

