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
#'   one or more dimension axes), then call `as_list()` to retrieve everything
#'   for inclusion as the "cs" attribute in the Zarr node metadata.
#'
#' @docType class
#' @export
zarr_convention_cs <- R6::R6Class('zarr_convention_cs',
  inherit = zarr::zarr_convention,
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
    #'   that appears in the Zarr array's `dimension_names` metadata. If the
    #'   argument is `NULL` or an empty list, the call is a no-op.
    #' @param name Optional character string. Descriptive name for the CRS.
    #' @param id Optional. Convention `proj` attributes (`proj:code`,
    #'   `proj:wkt2`, or `proj:projjson`) providing the formal OGC description
    #'   of the CRS.
    #' @param geolocation Optional list. Geolocation grid definition for
    #'   curvilinear grids. This should only be included for CRS's that
    #'   represent a planar coordinate system.
    #' @return Self, invisibly.
    add_crs = function(axes, name = NULL, id = NULL, geolocation = NULL) {
      if (is.null(axes) || !length(axes)) return()
      if (!is.list(axes) || is.null(names(axes)) || any(!nzchar(names(axes))))
        stop('Argument `axes` must be a non-empty named list', call. = FALSE)
      bad <- vapply(axes, function(a) !is.list(a) || is.null(a[['coordinates']]), logical(1L))
      if (any(bad))
        stop('Every element of `axes` must be a non-empty list', call. = FALSE)

      crs <- list(axes = axes)
      if (!is.null(name)) {
        if (!is.character(name) || length(name) != 1L || !nzchar(name))
          stop('Argument `name` must be a non-empty character string', call. = FALSE)
        crs$name <- name
      }
      if (!is.null(id)) crs$id <- id
      if (!is.null(geolocation)) crs$geolocation <- geolocation

      private$.crs <- c(private$.crs, list(crs))
      invisible(self)
    },

    #' @description Retrieve the `cs` attributes as a list.
    #' @return A `list` with the updated attributes for this convention.
    as_list = function() {
      if (!length(private$.crs))
        stop('At least one CRS must be added via `add_crs()`', call. = FALSE)

      cs <- list(crs = private$.crs)
      if (length(private$.name)) cs <- c(list(name = private$.name), cs)
      cs
    },

    #' @description Build an axis definition.
    #' @param coordinates A list of coordinate-set definitions, each produced by
    #'   `coordinates()`. Must have at least one element.
    #' @param abbreviation Optional character string. Axis abbreviation, e.g.
    #'   `"X"`, `"Y"`, `"Z"`, or `"T"`.
    #' @param direction Optional character string. Direction of increasing
    #'   coordinate values taken from Table 48 of the OGC standard "Referencing
    #'   by Coordinates" (e.g. `"EAST"`, `"NORTH"`, `"UP"`, `"FUTURE"`).
    #' @return A named list representing one axis entry.
    axis = function(coordinates, abbreviation = NULL, direction = NULL) {
      if (!is.list(coordinates) || !length(coordinates))
        stop('Argument `coordinates` must be a non-empty list', call. = FALSE)

      axis <- list(coordinates = coordinates)
      if (!is.null(abbreviation)) {
        if (!is.character(abbreviation) || length(abbreviation) != 1L)
          stop('Argument `abbreviation` must be a single character string', call. = FALSE)
        axis$abbreviation <- abbreviation
      }
      if (!is.null(direction)) {
        dir_upper <- toupper(direction)
        if (!dir_upper %in% AxisDirection)
          stop('Argument `direction` must be a value from the `AxisDirection` table', call. = FALSE)
        axis$direction <- dir_upper
      }
      axis
    },

    #' @description Build a coordinate-set definition for use in `axis()`.
    #' @param values A values definition produced by one of `values_regular()`,
    #'   `values_explicit()`, or `values_external()`.
    #' @param name Optional, character string. Descriptive name for this set of
    #'   coordinates.
    #' @param unit Optional, character string. Unit of measure (e.g.
    #'   `"degrees_east"`, `"m"`, `"1"`).
    #' @param boundaries Optional boundaries definition produced by
    #'   `boundaries_regular()` or `values_external()`.
    #' @param parametric Optional parametric definition produced by
    #'   `parametric()`.
    #' @param time Optional time definition produced by `time()`.
    #' @return A named list representing one coordinates entry.
    coordinates = function(values, name = NULL, unit = NULL,
                           boundaries = NULL, parametric = NULL, time = NULL) {
      if (!is.list(values) || is.null(values[['regular']]) &&
          is.null(values[['explicit']]) && is.null(values[['external']]))
        stop('Argument `values` is malformed', call. = FALSE)

      coords <- list(values = values)
      if (!is.null(name)) {
        if (!is.character(name) || length(name) != 1L || !nzchar(name))
          stop('Argument `name` must be a non-empty character string', call. = FALSE)
        coords$name <- name
      }
      if (is.null(time) && !is.null(unit)) {
        if (!is.character(unit) || length(unit) != 1L)
          stop('Argument `unit` must be a single character string', call. = FALSE)
        if (unit != '-')
          coords$unit <- unit
      }
      if (!is.null(time))       coords$time       <- time
      if (!is.null(boundaries)) coords$boundaries <- boundaries
      if (!is.null(parametric)) coords$parametric <- parametric
      coords
    },

    #' @description Regularly-spaced coordinate values.
    #' @param start Numeric. The coordinate value at shape index 0.
    #' @param increment Numeric. The constant spacing between successive values.
    #'   May be negative for decreasing coordinates (e.g. north-to-south
    #'   latitudes); it cannot be 0.
    #' @return A `values` list with a `regular` element.
    values_regular = function(start, increment) {
      if (!is.numeric(start) || length(start) != 1L)
        stop('Argument `start` must be a single numeric value', call. = FALSE)
      if (!is.numeric(increment) || length(increment) != 1L || increment == 0)
        stop('Argument `increment` must be a single non-zero numeric value', call. = FALSE)
      list(regular = c(start, increment))
    },

    #' @description Explicitly listed coordinate values.
    #' @param values A vector of coordinate values. May be numeric, integer, or
    #'   character.
    #' @return A `values` list with an `explicit` element.
    values_explicit = function(values) {
      if (!length(values))
        stop('Argument `values` must be a non-empty vector', call. = FALSE)
      list(explicit = as.list(values))
    },

    #' @description Coordinate values stored in an external array
    #' @param node Character string. Path to the 1-dimensional Zarr array
    #'   containing the coordinate values, relative to the referring node.
    #' @param uri Optional character string. URI of an external store. Omit for
    #'   arrays in the same local store.
    #' @return A `values` list with an `external` element.
    values_external = function(node, uri) {
      ref <- zarr::zarr_convention_ref$new()
      ref$set(node, uri)
      list(external = list(ref = ref$as_list()))
    },

    #' @description Regularly-spaced cell-boundary values.
    #'
    #'   The two values give the offset *below* and *above* the coordinate value
    #'   that define the extent of each cell, in the same unit as the
    #'   coordinates. Both offsets are expressed as positive magnitudes; the
    #'   convention interprets "below" as the lower-valued boundary and "above"
    #'   as the higher-valued boundary regardless of the sign of the axis
    #'   increment.
    #' @param below,above Numeric. Positive offset from the coordinate value to
    #'   the lower and upper boundary, respectively.
    #' @return A `boundaries` list with a `regular` element.
    boundaries_regular = function(below, above) {
      if (!is.numeric(below) || length(below) != 1L || below < 0)
        stop('Argument `below` must be a single non-negative numeric value', call. = FALSE)
      if (!is.numeric(above) || length(above) != 1L || above < 0)
        stop('Argument `above` must be a single non-negative numeric value', call. = FALSE)
      list(regular = c(below, above))
    },

    #' @description Provides the time reference information needed to interpret
    #'   numeric coordinate values on a temporal axis.
    #' @param unit Character string. The time unit, e.g. `"days"`, `"hours"`,
    #'   `"seconds"`.
    #' @param epoch Character string. The reference date/time in ISO 8601
    #'   format, e.g. `"1970-01-01"`.
    #' @param calendar Optional character string. The CF calendar name, e.g.
    #'   `"standard"`, `"360_day"`. Defaults to `"proleptic_gregorian"` when
    #'   omitted.
    #' @return A `time` list.
    time = function(unit, epoch, calendar = NULL) {
      if (!is.character(unit)  || length(unit)  != 1L || !nzchar(unit))
        stop('`unit` must be a non-empty character string', call. = FALSE)
      if (!is.character(epoch) || length(epoch) != 1L || !nzchar(epoch))
        stop('`epoch` must be a non-empty character string', call. = FALSE)
      tm <- list(unit = unit, epoch = epoch)
      if (is.null(calendar)) calendar <- "proleptic_gregorian"
      else if (!is.character(calendar) || !nzchar(calendar[1L]))
        stop('`calendar` must be a non-empty character string', call. = FALSE)
      tm$calendar <- calendar[1L]
      tm
    },

    #' @description Records the CF formula name and the set of formula terms
    #'   needed to derive physical coordinates from the stored parametric index
    #'   coordinates. This object sits alongside `values` inside a
    #'   `coordinates()` call; `values` provides the stored parametric
    #'   coordinates (e.g. `s_rho`), while `parametric` provides the machinery
    #'   to recover the physical coordinates (e.g. depth in metres).
    #' @param formula Character string. The CF `standard_name` of the parametric
    #'   coordinate formula, e.g. `"ocean_s_coordinate_g2"`.
    #' @param terms A named list of formula term values. Each element must be a
    #'   `values` attribute object. Scalar or short constants should use
    #'   explicit coordinate values; full-length arrays should use external Zarr
    #'   arrays.
    #' @return A `parametric` list.
    parametric = function(formula, terms) {
      if (!is.character(formula) || length(formula) != 1L || !nzchar(formula))
        stop('`formula` must be a non-empty character string', call. = FALSE)
      if (!is.list(terms) || !length(terms) || is.null(names(terms)) || any(!nzchar(names(terms))))
        stop('`terms` must be a non-empty named list of values objects', call. = FALSE)

      bad <- vapply(terms, function(t) is.null(t[['regular']]) && is.null(t[['explicit']]) && is.null(t[['external']]), logical(1L))
      if (any(bad))
        stop('Malformed `terms` elements', call. = FALSE)

      list(formula = formula, terms = terms)
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
