#' GeoZarr domain object
#'
#' @description This class implements a GeoZarr domain object. A GeoZarr domain
#'   object is a `zarr_domain` descendant object identifying groups and arrays
#'   in a `zarr` object that are formatted using GeoZarr conventions.
#'
#'   This domain supports the standard conventions for GeoZarr data sets,
#'   specifically the `cs` and `spatial` conventions, as well as two other
#'   formats for geospatial Zarr data that predate GeoZarr: the format used by
#'   the Python XArray library (Zarr v.2 and v.3) and the NCZarr format (Zarr
#'   v.2).
#' @docType class
#' @export
zarr_domain_geozarr <- R6::R6Class('zarr_domain_geozarr',
  inherit = zarr::zarr_domain,
  cloneable = FALSE,
  private = list(
    is_xarray_group = function(name, metadata, parent, store) {
      if (metadata$node_type == 'array')
        return(FALSE)

      # List immediate children of this group
      prefix <- if (is.null(parent)) ''
                else paste0(parent$prefix, name, '/')
      children <- store$list_dir(prefix)
      if (!length(children)) return(FALSE)

      # Fetch metadata for each child, keep only arrays (skip sub-groups)
      child_meta <- lapply(children, \(child) {
        childprefix <- paste0(prefix, child, '/')
        store$get_metadata(childprefix)
      })
      child_meta <- Filter(\(m) !is.null(m) && m$node_type == "array", child_meta)
      if (length(child_meta) == 0L) return(FALSE)

      # Every array must have dimension names, in either v2 or v3 form
      all(vapply(child_meta, \(m) {
        !is.null(m$dimension_names) ||             # v3 native field
        !is.null(m$attributes$`_ARRAY_DIMENSIONS`) # v2 attribute
      }, logical(1L)))
    },

    # Detect if the referenced array is XArray formatted. Dimension variables are
    # excluded.
    is_xarray_array = function(name, metadata, parent, store) {
      # XArray array must have a parent
      if (is.null(parent) || metadata$node_type == 'group')
        return(FALSE)

      dims <- metadata$dimension_names %||% metadata$attributes$`_ARRAY_DIMENSIONS`
      if (is.null(dims) || (len <- length(dims)) == 0L || (len == 1L && dims[1L] == name))
        return(FALSE)

      siblings <- store$list_dir(parent$prefix)
      any(dims %in% siblings)
    }
  ),
  public = list(
    #' @description Create a new GeoZarr domain instance. The GeoZarr domain
    #'   instance manages the groups and arrays in the Zarr store that it refers
    #'   to. This instance provides access to all objects in the Zarr store.
    #' @returns A `zar_domain_geozarr` object.
    initialize = function() {
      super$initialize('GeoZarr')
    },

    #' @description This method will create a `geozarr_array` for an array node
    #'   and a `geozarr_group` for a group node with GeoZarr conventions
    #'   declared in its attributes. Either the "spatial" or "cs" convention
    #'   has to be declared or the Zarr store has to be formatted using XArray
    #'   or NCZarr or this domain will decline to manage the node.
    #' @param name The name of the node.
    #' @param metadata List with the metadata of the node.
    #' @param parent The parent node of this new node. May be `NULL` for a root
    #'   node.
    #' @param store The store to persist data in.
    #' @return A `geozarr_array` or `geozarr_group` instance if supported,
    #'   `FALSE` otherwise.
    build = function(name, metadata, parent, store) {
      conv <- metadata$attributes$zarr_conventions
      if (is.null(conv)) {
        # Check for XArray
        if (private$is_xarray_array(name, metadata, parent, store)) {
          return(geozarr_array$new(name, metadata, parent, store))
        }

        # Check for NCZarr

        # No fun
        return(FALSE)
      }

      gz_conv <- GeoZarr.options$conventions
      for (cv in seq_along(conv)) {
        if (conv[[cv]]$name %in% gz_conv$name) {
          if (metadata$node_type == 'array')
            return(geozarr_array$new(name, metadata, parent, store))
          else
            return(geozarr_group$new(name, metadata, parent, store))
        }
      }

      # `spatial` convention: array may use parent attributes
      if (metadata$node_type == 'array' && inherits(parent, 'geozarr_group') &&
          'spatial' %in% parent$attributes$zarr_conventions)
        return(geozarr_array$new(name, metadata, parent, store))

      FALSE
    }
  )
)

# ==================== Helper functions ========================================

# This function will take the valid metadata for a Zarr array, the
# [CoordinateSystem] instance of a new `geozarr` array to create, optionally the
# path relative to the location of the new `geo_zarr` array for ther group that
# stores any external arrays with coordinate values. The function will then set
# the proper convention attributes based on the coordinate system and return the
# updated metadata.
.geozarr_set_convention <- function(metadata, coord_sys, external_group, registration = 'pixel') {
  meta <- metadata
  atts <- meta$attributes %||% list()
  axes <- coord_sys$axes

  # Drop any existing information
  meta$dimension_names <- NULL
  atts$zarr_conventions <- NULL
  if (length(atts)) {
    atts <- atts[!startsWith(names(atts), c('spatial:', 'proj:'))] # Drop any old spatial and proj elements
    atts$cs <- NULL # Remove any previous cs information
  }

  # dimension_names
  meta <- append(meta, list(dimension_names = vapply(axes, function(ax) ax$name, character(1L), USE.NAMES = FALSE)))

  # Axis abbreviation
  ax_abbr <- vapply(axes, function(ax) ax$abbreviation, FUN.VALUE = character(1), USE.NAMES = FALSE)
  X_axis <- which(ax_abbr == 'X')
  Y_axis <- which(ax_abbr == 'Y')
  if (!length(X_axis) && !length(Y_axis))
    stop('Cannot convert to GeoZarr: No X and/or Y axes found', call. = FALSE)

  # Set GeoZarr convention attributes
  if (length(X_axis) && length(Y_axis) && length(ax_abbr) <= 3L &&
      !('Z' %in% ax_abbr) && !('T' %in% ax_abbr) &&
      inherits(axes[[X_axis]]$coordinates$values, 'CoordinateValuesNumericPacked') && # == numeric & regular
      inherits(axes[[Y_axis]]$coordinates$values, 'CoordinateValuesNumericPacked') &&
      axes[[Y_axis]]$coordinates$values$raw[2L] < 0) {                                # == Y values descending
    # spatial convention
    # X + Y, optionally a band, no others, and X + Y coordinates are numeric and regular
    spatial <- zarr_conv_spatial$new()
    atts <- spatial$register(atts)

    dimensions <- c(axes[[Y_axis]]$name, axes[[X_axis]]$name)
    spatial$dimensions <- dimensions
    spatial$set_coordinates(shape = c(axes[[X_axis]]$length, axes[[Y_axis]]$length),
                            x = axes[[X_axis]]$coordinates$values$raw,
                            y = axes[[Y_axis]]$coordinates$values$raw,
                            registration = registration)

    atts <- c(atts, spatial$as_list())
  } else {
    # cs convention
    cs_conv <- zarr_convention_cs$new()
    atts    <- cs_conv$register(atts)

    # Direction lookup by axis abbreviation
    cs_direction <- c(X = 'EAST', Y = 'NORTH', Z = 'UP', T = 'FUTURE', OTHER = 'OTHER')

    axis_defs <- lapply(axes, function(ax) {
      # Values
      values <- ax$coordinates$raw
      values_def <- if (inherits(ax$coordinates, 'CoordinatesPacked'))
        cs_conv$values_regular(values[1L], values[2L])
      else if (ax$length <= GeoZarr.options$max_explicit)
        cs_conv$values_explicit(values)
      else
        # External coordinate values: Write coordinate values to an external array.
        # The name of the external array is `<axis_name>_coord`. The actual writing
        # to the external array should be done in the calling code.
        cs_conv$values_external(paste0(external_group, '/', paste0(ax$name, '_coord')))

      # Time
      time_def <- if (inherits(ax$coordinates, 'CoordinatesTime')) {
        def <- strsplit(ax$coordinates$time$calendar$definition, ' ', fixed = TRUE)[[1L]]
        cs_conv$time(unit = def[1L], epoch = def[3L], calendar = ax$coordinates$time$calendar$calendar)
      } else NULL

      # Coordinates and axis
      coords_def <- cs_conv$coordinates(values_def, unit = ax$coordinates$unit, time = time_def)
      abbr <- ax$abbreviation
      direction  <- cs_direction[[abbr]]
      if (abbr == 'OTHER') abbr <- ''
      cs_conv$axis(list(coords_def), abbreviation = abbr, direction = direction)
    })

    # Group axes into separate CRS objects by axis category
    cs_conv$add_crs(axes = axis_defs[c(X_axis, Y_axis)])
    cs_conv$add_crs(axes = axis_defs[which(ax_abbr == 'Z')])
    cs_conv$add_crs(axes = axis_defs[which(ax_abbr == 'T')])
    cs_conv$add_crs(axes = axis_defs[which(ax_abbr == 'OTHER')])

    atts <- c(list(cs = cs_conv$as_list()), atts)
  }

  meta$attributes <- NULL # Remove any previous attributes
  meta$attributes <- atts
  meta
}
