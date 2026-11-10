#' GeoZarr Group
#'
#' @description This class implements a GeoZarr group. A GeoZarr group is a
#'   group in the hierarchy of a Zarr object having GeoZarr attributes. A
#'   GeoZarr group is a container for other groups and arrays.
#' @docType class
#' @export
geozarr_group <- R6::R6Class('geozarr_group',
  inherit = zarr::zarr_group,
  cloneable = FALSE,
  private = list(
    # The multiscales agent of this group, FALSE when there is none, NULL when
    # not yet determined.
    .multiscales = NULL,

    # Print GeoZarr details as part of printing a group.
    print_details = function() {

    },

    # Remove domain attributes prior to printing this geozarr_group
    display_attributes = function() {
      atts <- private$.metadata[['attributes']]
      if (length(atts)) {
        nms <- names(atts)
        atts[!(startsWith(nms, 'spatial:') | (nms %in% 'zarr_conventions'))]
      } else list()
    }
  ),
  public = list(
    #' @description Open a GeoZarr group in a Zarr hierarchy.
    #' @param name The name of the group. For a root group, this is the empty
    #'   string `""`.
    #' @param metadata List with the metadata of the group.
    #' @param parent The parent Zarr group instance of this new group, can be
    #'   missing or `NULL` for the root group.
    #' @return An instance of `geozarr_group`.
    initialize = function(name, metadata, parent) {
      super$initialize(name, metadata, parent)
      private$.domain <- 'GeoZarr'
    }
  ),
  active = list(
    #' @field multiscales (read-only) The [zarr_convention_multiscales]
    #'   instance with the multiscales layout of this group, or `NULL` when the
    #'   group does not declare the "multiscales" convention. A warning is
    #'   given, once, when the `multiscales` attribute is not valid; the layout
    #'   is then ignored.
    multiscales = function(value) {
      if (!missing(value))
        stop("Can't set the multiscales layout of a group", call. = FALSE)

      if (is.null(private$.multiscales)) {
        ms <- private$.metadata$attributes$multiscales
        private$.multiscales <-
          if (is.null(ms) || !.declares(private$.metadata, 'multiscales')) FALSE
          else tryCatch(zarr_convention_multiscales$new(ms), error = function(e) {
            warning('Ignoring "multiscales" of group ', self$path, ': ', conditionMessage(e), call. = FALSE)
            FALSE
          })
      }
      if (isFALSE(private$.multiscales)) NULL else private$.multiscales
    }
  )
)
