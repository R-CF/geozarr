#' GeoZarr Array
#'
#' @description This class implements a GeoZarr array. A GeoZarr array is like a
#'   regular Zarr array but it has attributes and/or associated Zarr arrays that
#'   provide a coordinate system for the array.
#' @docType class
#' @export
geozarr_array <- R6::R6Class('geozarr_array',
  inherit = zarr_array,
  cloneable = FALSE,
  private = list(
    # Print GeoZarr details as part of printing a group.
    print_details = function() {
      cat('\nCoordinate system:\n')
    },

    # Remove domain attributes prior to printing this geozarr_array
    display_attributes = function() {
      atts <- private$.metadata[['attributes']]
      nms <- names(atts)
      atts[!(startsWith(nms, 'spatial:') | (nms %in% 'zarr_conventions'))]
    }
  ),
  public = list(
    #' @description Initialize a new GeoZarr array. The array must
    #'   already exist in the store
    #' @param name The name of the GeoZarr array.
    #' @param metadata List with the metadata of the array.
    #' @param parent The parent `zarr_group` instance of this new array, can be
    #'   missing or `NULL` if the Zarr object should have just this array.
    #' @param store The [zarr_store] instance to persist data in.
    #' @return An instance of `geozarr_array`.
    initialize = function(name, metadata, parent, store) {
      super$initialize(name, metadata, parent, store)
      private$.domain <- 'GeoZarr'
    }
  )
)
