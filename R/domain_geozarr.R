#' GeoZarr domain object
#'
#' @description This class implements a GeoZarr domain object. A GeoZarr domain
#'   object is a `zarr_domain` descendant object identifying groups and arrays
#'   in a `zarr` object that are formatted using GeoZarr conventions.
#' @docType class
#' @export
zarr_domain_geozarr <- R6::R6Class('zarr_domain_geozarr',
  inherit = zarr::zarr_domain,
  cloneable = FALSE,
  private = list(

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
    #'   declared in its attributes. Either the "spatial:" or "crs" convention
    #'   has to be declared or this domain will decline to manage the node.
    #' @param name The name of the node.
    #' @param metadata List with the metadata of the node.
    #' @param parent The parent node of this new node. May be `NULL` for a root
    #'   node.
    #' @param store The store to persist data in.
    #' @return A `geozarr_array` or `geozarr_group` instance if GeoZarr
    #'   conventions are found, `FALSE` otherwise.
    build = function(name, metadata, parent, store) {
      conv <- metadata$attributes$zarr_conventions
      if (is.null(conv)) return(FALSE)

      gz_conv <- GeoZarr.options$conventions
      for (cv in seq_along(conv)) {
        if (conv[[cv]]$name %in% gz_conv$name) {
          if (metadata$node_type == 'array')
            return(geozarr_array$new(name, metadata, parent, store))
          else
            return(geozarr_group$new(name, metadata, parent, store))
        }
      }

      # `spatial:` convention: array may use parent attributes
      if (metadata$node_type == 'array' && inherits(parent, 'geozarr_group') &&
          'spatial:' %in% parent$attributes$zarr_conventions)
        return(geozarr_array$new(name, metadata, parent, store))

      FALSE
    }
  )
)
