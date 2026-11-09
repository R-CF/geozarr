#' GeoZarr "geolocation" convention
#'
#' @description This class implements the "geolocation" convention. The
#'   convention attaches references to geolocation arrays to a Zarr array whose
#'   own coordinate system lacks a simple mapping to a coordinate reference
#'   system, such as a level-1 satellite swath or a model grid on a rotated or
#'   tripolar grid. The geolocation arrays give, for every element of the
#'   referring array, the coordinates in a geodetic and/or a planar CRS.
#'
#'   The convention is registered via the following CMO:
#'
#' ```{r schema, eval = FALSE}
#' {
#'   "schema_url": "https://raw.githubusercontent.com/R-CF/zarr_convention_geolocation/refs/tags/v2.0.0/schema.json",
#'   "spec_url":   "https://github.com/R-CF/zarr_convention_geolocation/blob/v2.0.0/README.md",
#'   "uuid":       "bb9ee930-8c60-4c47-ad6b-8daa558987ed",
#'   "name":       "geolocation",
#'   "description": "Convention for storing geolocation arrays"
#' }
#' ```
#'
#'   The `geolocation` object has the following structure:
#'
#' ```json
#' {
#'   "geolocation": {
#'     "geodetic": {
#'       "x":  { "values":     { "ref": { "node": "../longitude" } },
#'               "boundaries": { "ref": { "node": "../longitude_bnds" } } },
#'       "y":  { "values":     { "ref": { "node": "../latitude" } },
#'               "boundaries": { "ref": { "node": "../latitude_bnds" } } },
#'       "id": { "proj:code": "EPSG:4326" }
#'     },
#'     "planar": { ... }
#'   }
#' }
#' ```
#'
#'   The convention does not determine where the `geolocation` object is placed
#'   in the attributes; that follows from the principal convention providing
#'   the coordinates of the array. With the "spatial" convention the object is
#'   placed at the top level of the attributes; with the "cs" convention it is
#'   nested in a CRS object, by passing this instance to
#'   `zarr_convention_cs$add_crs()`.
#'
#'   The `values` and optional `boundaries` fields of `x` and `y` always hold
#'   "ref" convention objects, so
#'   registering this convention with `register()` also registers the "ref"
#'   convention, as well as the "proj" convention when any `id` is present.
#'   Call `register()` after the geolocation arrays have been set.
#' @docType class
#' @export
zarr_convention_geolocation <- R6::R6Class('zarr_convention_geolocation',
  inherit = zarr::zarr_convention,
  cloneable = FALSE,
  private = list(
    # Arrays objects, keyed by "geodetic" and/or "planar".
    .arrays = list()
  ),
  public = list(
    #' @description Create a new instance of a "geolocation" convention agent.
    #' @param geolocation Optional. A `geolocation` object as read from the
    #'   attributes of a Zarr node, in the form of a named `list` with elements
    #'   "geodetic" and/or "planar".
    #' @details An error is thrown if argument `geolocation` is given but it
    #'   is not a named list with elements "geodetic" and/or "planar", each
    #'   having elements "x" and "y" with a "values" field and, for both or for
    #'   neither, a "boundaries" field, all of which are `ref` objects with a
    #'   "node" field.
    #' @return A new instance of a "geolocation" convention agent.
    initialize = function(geolocation) {
      super$initialize(
        name   = 'geolocation',
        schema = 'https://raw.githubusercontent.com/R-CF/zarr_convention_geolocation/refs/tags/v2.0.0/schema.json',
        uuid   = 'bb9ee930-8c60-4c47-ad6b-8daa558987ed'
      )
      private$.spec        <- 'https://github.com/R-CF/zarr_convention_geolocation/blob/v2.0.0/README.md'
      private$.description <- 'Convention for storing geolocation arrays'

      if (!missing(geolocation)) {
        ok_ref <- function(r) is.list(r) && is.character(r$ref$node) && length(r$ref$node) == 1L && nzchar(r$ref$node)
        ok_crd <- function(c) is.list(c) && ok_ref(c$values) && (is.null(c$boundaries) || ok_ref(c$boundaries))
        ok <- is.list(geolocation) && length(geolocation) &&
              !is.null(names(geolocation)) && !anyDuplicated(names(geolocation)) &&
              all(names(geolocation) %in% c('geodetic', 'planar')) &&
              all(vapply(geolocation, function(a) is.list(a) && ok_crd(a$x) && ok_crd(a$y) &&
                                                  is.null(a$x$boundaries) == is.null(a$y$boundaries), logical(1L)))
        if (!ok)
          stop('Argument `geolocation` is misformed', call. = FALSE)
        private$.arrays <- geolocation
      }
    },

    #' @description Register the use of this convention, and of the "ref" and
    #'   "proj" conventions that it composes, in the attributes of a Zarr
    #'   object.
    #' @param attributes A `list` with Zarr attributes for a group or array.
    #' @param brief Logical flag to indicate if the registration should only
    #'   include the name and the schema URL or all details (default).
    #' @return The updated attributes.
    register = function(attributes, brief = FALSE) {
      attributes <- super$register(attributes, brief)
      attributes <- zarr::zarr_convention_ref$new()$register(attributes, brief)
      if (any(vapply(private$.arrays, function(a) !is.null(a$id), logical(1L))))
        attributes <- zarr_conv_proj$new()$register(attributes, brief)
      attributes
    },

    #' @description Set the geolocation arrays for geodetic or planar
    #'   coordinates. Setting the same `kind` again replaces the earlier
    #'   setting.
    #' @param kind Character string, either "geodetic" or "planar".
    #' @details An error is thrown when only one of `x_bounds` and `y_bounds`
    #'   is given, or when a path or `uri` is not a valid reference.
    #' @param x,y Character strings. Paths to the Zarr arrays holding the X and
    #'   Y coordinates, respectively. For "geodetic" these are the longitude and
    #'   latitude arrays. The paths are relative to the referring node when `uri`
    #'   is missing, absolute from the root of the store in `uri` otherwise.
    #' @param uri Optional character string. URI of an external Zarr store that
    #'   holds all arrays. Omit for arrays in the same store as the referring
    #'   node.
    #' @param id Optional. An instance of [zarr_conv_proj] identifying the CRS
    #'   of the coordinates in the `x` and `y` arrays. This should be a geodetic
    #'   CRS for `kind = "geodetic"`.
    #' @param x_bounds,y_bounds Optional character strings. Paths to the Zarr
    #'   arrays holding the cell vertices of the X and Y coordinates. Give both
    #'   or neither.
    #' @return Self, invisibly.
    set = function(kind, x, y, uri = NULL, id = NULL, x_bounds = NULL, y_bounds = NULL) {
      kind <- match.arg(kind, c('geodetic', 'planar'))
      if (!is.null(id) && !inherits(id, 'zarr_conv_proj'))
        stop('Argument `id` must be an instance of `zarr_conv_proj`', call. = FALSE)
      if (is.null(x_bounds) != is.null(y_bounds))
        stop('Arguments `x_bounds` and `y_bounds` must both be given, or neither', call. = FALSE)

      ref <- function(node) {
        r <- zarr::zarr_convention_ref$new()
        r$set(node, uri)
        list(ref = r$as_list())
      }
      crd <- function(values, bounds)
        if (is.null(bounds)) list(values = ref(values))
        else list(values = ref(values), boundaries = ref(bounds))
      arrays <- list(x = crd(x, x_bounds), y = crd(y, y_bounds))
      if (!is.null(id)) arrays$id <- id$as_list()

      private$.arrays[[kind]] <- arrays
      invisible(self)
    },

    #' @description Clear the geolocation arrays that have been set.
    clear = function() {
      private$.arrays <- list()
    },

    #' @description Retrieve the `geolocation` object as a list.
    #' @return A `list` with elements "geodetic" and/or "planar".
    as_list = function() {
      if (!length(private$.arrays))
        stop('Geodetic and/or planar geolocation arrays must be set with `set()`', call. = FALSE)
      private$.arrays[intersect(c('geodetic', 'planar'), names(private$.arrays))]
    }
  )
)
