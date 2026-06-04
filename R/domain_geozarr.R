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
      has_dims <- vapply(child_meta, \(m) {
        !is.null(m$dimension_names) ||             # v3 native field
        !is.null(m$attributes$`_ARRAY_DIMENSIONS`) # v2 attribute
      }, logical(1L))
      if (!all(has_dims)) return(FALSE)

      # At least one array must have a `coordinates` attribute
      # has_coords <- vapply(child_meta, \(m) {
      #   !is.null(m$attributes$coordinates)
      # }, logical(1L))
      #
      # any(has_coords)

      TRUE
    },

    # Detect if the rferenced array is XArray formatted. Dimension variables are
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
