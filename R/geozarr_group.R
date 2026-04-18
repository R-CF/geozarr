#' GeoZarr Group
#'
#' @description This class implements a GeoZarr group. A GeoZarr group is a
#'   group in the hierarchy of a Zarr object having GeoZarr attributes. A
#'   GeoZarr group is a container for other groups and arrays.
#' @docType class
#' @export
geozarr_group <- R6::R6Class('geozarr_group',
  inherit = zarr_group,
  cloneable = FALSE,
  private = list(
    # Print GeoZarr details as part of printing a group.
    print_details = function() {

    },

    # Remove domain attributes prior to printing this geozarr_group
    display_attributes = function() {
      atts <- private$.metadata[['attributes']]
      nms <- names(atts)
      atts[!(startsWith(nms, 'spatial:') | (nms %in% 'zarr_conventions'))]
    }
  ),
  public = list(
    #' @description Open a GeoZarr group in a Zarr hierarchy.
    #' @param name The name of the group. For a root group, this is the empty
    #'   string `""`.
    #' @param metadata List with the metadata of the group.
    #' @param parent The parent Zarr group instance of this new group, can be
    #'   missing or `NULL` for the root group.
    #' @param store The [zarr_store] instance to persist data in.
    #' @return An instance of `geozarr_group`.
    initialize = function(name, metadata, parent, store) {
      super$initialize(name, metadata, parent, store)
      private$.domain <- 'GeoZarr'
    }
  )
)
