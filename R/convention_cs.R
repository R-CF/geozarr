#' GeoZarr "cs" convention
#'
#' @description This class implements the GeoZarr "cs" (coordinate set)
#'   convention. The convention attaches a full coordinate system to a Zarr
#'   array by recording, for each dimension, the axis abbreviation and
#'   direction, one or more sets of coordinate values and their optional
#'   cell-boundary values, and, where applicable, the parametric formula and
#'   its terms needed to derive physical coordinates from stored index
#'   coordinates.
#'
#'   The convention is registered via the following CMO:
#'
#' ```{r schema, eval = FALSE}
#' {
#'   "schema_url": "https://raw.githubusercontent.com/R-CF/zarr_convention_cs/main/schema.json",
#'   "spec_url":   "https://raw.githubusercontent.com/R-CF/zarr_convention_cs/main/README.md",
#'   "uuid":       "e4dbf0b7-7a00-4ce6-b23e-484292014ab4",
#'   "name":       "cs",
#'   "description": "Coordinate set convention for Zarr arrays"
#' }
#' ```
#'
#'   The `cs` attribute written to the array metadata has the following
#'   structure (simplified):
#'
#' ```json
#' {
#'   "cs": {
#'     "crs": [
#'       {
#'         "axes": {
#'           "<dimension_name>": {
#'             "abbreviation": "X",
#'             "direction":    "EAST",
#'             "coordinates":  [ { "unit": "degrees",
#'                                 "values": { "regular": [0.0, 0.5] } } ]
#'           }
#'         }
#'       }
#'     ]
#'   }
#' }
#' ```
#'
#'   Build a convention instance, add one or more CRS objects (each covering
#'   one or more dimension axes), then call `write()` to embed everything in
#'   the Zarr attributes list.
#'
#' @docType class
#' @export
zarr_conv_cs <- R6::R6Class('zarr_conv_cs',
  inherit = zarr_convention,
  cloneable = FALSE,
  private = list(
    # Optional top-level name for the coordinate set.
    .name = character(0),

    # List of CRS objects. Each element is the list that will be serialised
    # directly as a JSON object under cs$crs[].
    .crs = list()
  ),
  public = list(
    #' @description Create a new instance of a "cs" convention agent.
    #' @return A new instance of a "cs" convention agent.
    initialize = function() {
      super$initialize(
        name   = 'cs',
        schema = 'https://raw.githubusercontent.com/R-CF/zarr_convention_cs/main/schema.json',
        uuid   = 'e4dbf0b7-7a00-4ce6-b23e-484292014ab4'
      )
      private$.spec        <- 'https://raw.githubusercontent.com/R-CF/zarr_convention_cs/main/README.md'
      private$.description <- 'Coordinate set convention for Zarr arrays'
    },

    #' @description Add a CRS object to this coordinate set. Each CRS covers one
    #'   or more axes (dimensions). Multiple calls add further CRS objects to
    #'   the array, which is necessary when the array spans domains described by
    #'   separate OGC coordinate reference systems (e.g. a horizontal CRS plus a
    #'   vertical CRS plus a temporal CRS).
    #' @param axes A named list of axis definitions, keyed by the dimension name
    #'   that appears in the Zarr array's `dimension_names` metadata. Each
    #'   element is an instance of [CoordinateSystemAxis].
    #' @param name Optional character string. Descriptive name for the CRS.
    #' @param id Optional. Convention `proj` attributes (`proj:code`,
    #'   `proj:wkt2`, or `proj:projjson`) providing the formal OGC description
    #'   of the CRS.
    #' @param geolocation Optional list. Geolocation grid definition for
    #'   curvilinear grids. This should only be included for CRS's that
    #'   represent a planar coordinate system.
    #' @return Self, invisibly.
    add_crs = function(axes, name = NULL, id = NULL, geolocation = NULL) {
      if (!is.list(axes) || !length(axes) || is.null(names(axes)) || any(!nzchar(names(axes))))
        stop('`axes` must be a non-empty named list', call. = FALSE)
      bad <- vapply(axes, function(a) !is.list(a) || is.null(a[['coordinates']]), logical(1L))
      if (any(bad))
        stop('Every element of `axes` must be a non-empty list', call. = FALSE)

      crs <- list(axes = axes)
      if (!is.null(name)) {
        if (!is.character(name) || length(name) != 1L || !nzchar(name))
          stop('`name` must be a non-empty character string.', call. = FALSE)
        crs$name <- name
      }
      if (!is.null(id)) crs$id <- id
      if (!is.null(geolocation)) crs$geolocation <- geolocation

      private$.crs <- c(private$.crs, list(crs))
      invisible(self)
    },

    #' @description Write the `cs` attribute into a Zarr attributes list. The
    #'   CMO entry in `zarr_conventions` is written by the inherited
    #'   `register()` method and must be called separately before this method.
    #' @param attributes A `list` with Zarr attributes for an array.
    #' @return The updated attributes list.
    write = function(attributes) {
      if (!length(private$.crs))
        stop('At least one CRS must be added via `add_crs()` before writing.', call. = FALSE)

      cs <- list(crs = private$.crs)
      if (length(private$.name)) cs <- c(list(name = private$.name), cs)

      attributes$cs <- cs
      attributes
    }
  ),
  active = list(
    #' @field name Optional descriptive name for the coordinate set.
    name = function(value) {
      if (missing(value))
        private$.name
      else if (is.character(value) && length(value) == 1L && nzchar(value))
        private$.name <- value
      else
        stop('`name` must be a non-empty character string.', call. = FALSE)
    },

    #' @field crs (read-only) The list of CRS objects accumulated so far.
    crs = function(value) {
      if (missing(value))
        private$.crs
    }
  )
)

# -------------------------------------------------------------------------
# Helper constructors
#
# These functions produce the list structures that zarr_conv_cs$add_crs()
# and the other helpers expect. They are not exported because users
# interact with them only while building a zarr_conv_cs instance.
# -------------------------------------------------------------------------

#' Build an axis definition for use in [zarr_conv_cs]`$add_crs()`
#'
#' @param coordinates A list of coordinate-set definitions, each produced by
#'   `.cs_coordinates()`. Must have at least one element.
#' @param abbreviation Optional character string. Axis abbreviation, e.g.
#'   `"X"`, `"Y"`, `"Z"`, or `"T"`.
#' @param direction Optional character string. Direction of increasing
#'   coordinate values taken from Table 48 of the OGC standard "Referencing
#'   by Coordinates" (e.g. `"EAST"`, `"NORTH"`, `"UP"`, `"FUTURE"`).
#' @return A named list representing one axis entry.
#' @noRd
.cs_axis <- function(coordinates, abbreviation = NULL, direction = NULL) {
  if (!is.list(coordinates) || !length(coordinates))
    stop('`coordinates` must be a non-empty list', call. = FALSE)

  axis <- list(coordinates = coordinates)
  if (!is.null(abbreviation)) {
    if (!is.character(abbreviation) || length(abbreviation) != 1L)
      stop('`abbreviation` must be a single character string.', call. = FALSE)
    axis$abbreviation <- abbreviation
  }
  if (!is.null(direction)) {
    dir_upper <- toupper(direction)
    if (!dir_upper %in% AxisDirection)
      stop('`direction` must be a value from the `AxisDirection` table', call. = FALSE)
    axis$direction <- dir_upper
  }
  axis
}

#' Build a coordinate-set definition for use in `.cs_axis()`
#'
#' @param values A values definition produced by one of `.cs_values_regular()`,
#'   `.cs_values_explicit()`, or `.cs_values_external()`.
#' @param name Optional character string. Descriptive name for this set of
#'   coordinates.
#' @param unit Optional character string. Unit of measure (e.g.
#'   `"degrees_east"`, `"m"`, `"1"`).
#' @param boundaries Optional boundaries definition produced by
#'   `.cs_boundaries_regular()` or `.cs_boundaries_external()`.
#' @param parametric Optional parametric definition produced by
#'   `.cs_parametric()`.
#' @param time Optional time definition produced by `.cs_time()`.
#' @return A named list representing one coordinates entry.
#' @noRd
.cs_coordinates <- function(values, name = NULL, unit = NULL,
                             boundaries = NULL, parametric = NULL,
                             time = NULL) {
  if (!is.list(values) || is.null(values[['regular']]) &&
      is.null(values[['explicit']]) && is.null(values[['external']]))
    stop('`values` must be produced by `.cs_values_regular()`, ',
         '`.cs_values_explicit()`, or `.cs_values_external()`.', call. = FALSE)

  coords <- list(values = values)
  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || !nzchar(name))
      stop('`name` must be a non-empty character string.', call. = FALSE)
    coords$name <- name
  }
  if (!is.null(unit)) {
    if (!is.character(unit) || length(unit) != 1L)
      stop('`unit` must be a single character string.', call. = FALSE)
    coords$unit <- unit
  }
  if (!is.null(time))       coords$time       <- time
  if (!is.null(boundaries)) coords$boundaries <- boundaries
  if (!is.null(parametric)) coords$parametric <- parametric
  coords
}

#' Regularly-spaced coordinate values
#'
#' @param start Numeric. The coordinate value at shape index 0.
#' @param increment Numeric. The constant spacing between successive values.
#'   May be negative for decreasing coordinates (e.g. north-to-south
#'   latitudes).
#' @return A `values` list with a `regular` element.
#' @noRd
.cs_values_regular <- function(start, increment) {
  if (!is.numeric(start)     || length(start)     != 1L)
    stop('`start` must be a single numeric value.', call. = FALSE)
  if (!is.numeric(increment) || length(increment) != 1L || increment == 0)
    stop('`increment` must be a single non-zero numeric value.', call. = FALSE)
  list(regular = c(start, increment))
}

#' Explicitly listed coordinate values
#'
#' @param values A vector of coordinate values. May be numeric, integer, or
#'   character.
#' @return A `values` list with an `explicit` element.
#' @noRd
.cs_values_explicit <- function(values) {
  if (!length(values))
    stop('`values` must be a non-empty vector.', call. = FALSE)
  list(explicit = as.list(values))
}

#' Coordinate values stored in an external array
#'
#' @param node Character string. Path to the 1-dimensional Zarr array
#'   containing the coordinate values, relative to the referring node.
#' @param uri Optional character string. URI of an external store. Omit for
#'   arrays in the same local store.
#' @param attribute Optional character string. JSON Pointer to an attribute
#'   of the referenced node, when the values are stored as metadata rather
#'   than array data.
#' @return A `values` list with an `external` element.
#' @noRd
.cs_values_external <- function(node, uri = NULL, attribute = NULL) {
  if (!is.character(node) || length(node) != 1L || !nzchar(node))
    stop('`node` must be a non-empty character string giving the path to ',
         'the coordinate array.', call. = FALSE)
  ref <- list(node = node)
  if (!is.null(uri)) {
    if (!is.character(uri) || length(uri) != 1L || !nzchar(uri))
      stop('`uri` must be a non-empty character string.', call. = FALSE)
    ref$uri <- uri
  }
  if (!is.null(attribute)) {
    if (!is.character(attribute) || length(attribute) != 1L)
      stop('`attribute` must be a single character string (JSON Pointer).',
           call. = FALSE)
    ref$attribute <- attribute
  }
  list(external = list(ref = ref))
}

#' Regularly-spaced cell-boundary values
#'
#' The two values give the offset *below* and *above* the coordinate value
#' that define the extent of each cell, in the same unit as the coordinates.
#' Both offsets are expressed as positive magnitudes; the convention
#' interprets "below" as the lower-valued boundary and "above" as the
#' higher-valued boundary regardless of the sign of the axis increment.
#'
#' @param below Numeric. Positive offset from the coordinate value to the
#'   lower boundary.
#' @param above Numeric. Positive offset from the coordinate value to the
#'   upper boundary.
#' @return A `boundaries` list with a `regular` element.
#' @noRd
.cs_boundaries_regular <- function(below, above) {
  if (!is.numeric(below) || length(below) != 1L || below < 0)
    stop('`below` must be a single non-negative numeric value.', call. = FALSE)
  if (!is.numeric(above) || length(above) != 1L || above < 0)
    stop('`above` must be a single non-negative numeric value.', call. = FALSE)
  list(regular = c(below, above))
}

#' Cell-boundary values stored in an external array
#'
#' @param node Character string. Path to the Zarr array containing the
#'   boundary values, relative to the referring node.
#' @param uri Optional character string. URI of an external store.
#' @param attribute Optional character string. JSON Pointer into the node's
#'   attributes.
#' @return A `boundaries` list with an `external` element.
#' @noRd
.cs_boundaries_external <- function(node, uri = NULL, attribute = NULL) {
  if (!is.character(node) || length(node) != 1L || !nzchar(node))
    stop('`node` must be a non-empty character string giving the path to ',
         'the boundaries array.', call. = FALSE)
  ref <- list(node = node)
  if (!is.null(uri))       ref$uri       <- uri
  if (!is.null(attribute)) ref$attribute <- attribute
  list(external = list(ref = ref))
}

#' Parametric coordinate definition
#'
#' Records the CF formula name and the set of formula terms needed to derive
#' physical coordinates from the stored parametric index coordinates. This
#' object sits alongside `values` inside a `.cs_coordinates()` call;
#' `values` provides the stored parametric coordinates (e.g. `s_rho`), while
#' `parametric` provides the machinery to recover the physical coordinates
#' (e.g. depth in metres).
#'
#' @param formula Character string. The CF `standard_name` of the parametric
#'   coordinate formula, e.g. `"ocean_s_coordinate_g2"`.
#' @param terms A named list of formula term values. Each element must be a
#'   `values` object produced by `.cs_values_regular()`,
#'   `.cs_values_explicit()`, or `.cs_values_external()`. Scalar or short
#'   constants should use `.cs_values_explicit()`; full-length arrays should
#'   use `.cs_values_external()`.
#' @return A `parametric` list.
#' @noRd
.cs_parametric <- function(formula, terms) {
  if (!is.character(formula) || length(formula) != 1L || !nzchar(formula))
    stop('`formula` must be a non-empty character string.', call. = FALSE)
  if (!is.list(terms) || !length(terms) || is.null(names(terms)) || any(!nzchar(names(terms))))
    stop('`terms` must be a non-empty named list of values objects.', call. = FALSE)

  bad <- vapply(terms, function(t)
    is.null(t[['regular']]) && is.null(t[['explicit']]) && is.null(t[['external']]),
    logical(1L))
  if (any(bad))
    stop('Every element of `terms` must be produced by `.cs_values_*()`.',
         call. = FALSE)

  list(formula = formula, terms = terms)
}

#' Time axis definition
#'
#' Provides the time reference information needed to interpret numeric
#' coordinate values on a temporal axis.
#'
#' @param unit Character string. The time unit, e.g. `"days"`, `"hours"`,
#'   `"seconds"`.
#' @param epoch Character string. The reference date/time in ISO 8601 format, e.g. `"1970-01-01"`.
#' @param calendar Optional character string. The CF calendar name, e.g.
#'   `"proleptic_gregorian"`, `"360_day"`. Defaults to `"standard"` when
#'   omitted.
#' @return A `time` list.
#' @noRd
.cs_time <- function(unit, epoch, calendar = NULL) {
  if (!is.character(unit)  || length(unit)  != 1L || !nzchar(unit))
    stop('`unit` must be a non-empty character string.', call. = FALSE)
  if (!is.character(epoch) || length(epoch) != 1L || !nzchar(epoch))
    stop('`epoch` must be a non-empty character string.', call. = FALSE)
  tm <- list(unit = unit, epoch = epoch)
  if (!is.null(calendar)) {
    if (!is.character(calendar) || length(calendar) != 1L || !nzchar(calendar))
      stop('`calendar` must be a non-empty character string.', call. = FALSE)
    tm$calendar <- calendar
  }
  tm
}
