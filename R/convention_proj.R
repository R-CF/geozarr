#' GeoZarr "proj" convention
#'
#' @description This class implements the GeoZarr "proj" convention. In
#'   particular, the following convention is implemented here:
#'
#' ```{r schema, eval = FALSE}
#' {
#'   "schema_url": "https://raw.githubusercontent.com/zarr-conventions/geo-proj/refs/tags/v1/schema.json",
#'   "spec_url": "https://github.com/zarr-conventions/geo-proj/blob/v1/README.md",
#'   "uuid": "f17cb550-5864-4468-aeb7-f3180cfb622f",
#'   "name": "proj",
#'   "description": "Coordinate reference system information for geospatial data"
#' }
#' ```
#' @docType class
#' @export
zarr_conv_proj <- R6::R6Class('zarr_conv_proj',
  inherit = zarr::zarr_convention,
  cloneable = FALSE,
  private = list(
    # Optional: Character string giving an identifier composed of an authority
    # name and a unique identifier for that authority, separated by a colon,
    # such as "EPSG:4326".
    .code = '',

    # Optional: Well-known text strings describing coordinate reference
    # systems in the WKT2 format.
    .wkt2 = '',

    # Optional: Representation of the CRS in PROJJSON format.
    .projjson = ''
  ),
  public = list(
    #' @description Create a new instance of a "proj" convention agent.
    #' @param attributes Optional, a named `list` with one or more of the `proj`
    #'   attributes. The elements in the list must be one or more of "code",
    #'   "wkt2" and "projjson".
    #' @return A new instance of a "proj" convention agent.
    initialize = function(attributes) {
      super$initialize(name   = 'proj',
                       schema = 'https://raw.githubusercontent.com/zarr-conventions/geo-proj/refs/tags/v1/schema.json',
                       uuid   = 'f17cb550-5864-4468-aeb7-f3180cfb622f')
      private$.spec <- 'https://github.com/zarr-conventions/geo-proj/blob/v1/README.md'
      private$.description <- 'Spatial coordinate information'

      # Set attributes
      if (!missing(attributes)) {
        if (is.list(attributes) && all(names(attributes) %in% c('code', 'wkt2', 'projjson')))
          lapply(names(attributes), function(c) {eval(parse(text = paste0('private$.', c, ' <- \'', attributes[[c]], '\'')))})
        else
          stop('Argument `attributes` for `proj` convention is misformed', call. = FALSE)
      }
    },

    #' @description Write the data of this instance in the attributes of a Zarr
    #'   object.
    #' @param attributes A `list` with Zarr attributes for a group or array. The
    #'   properties will be written at the root level of `attributes`.
    #' @return The updated attributes.
    write = function(attributes) {
      if (!nzchar(private$.code) && !nzchar(private$.wkt2) && !nzchar(private$.projjson))
        stop('At least one of the attributes must be set', call. = FALSE)

      if (nzchar(private$.code))
        attributes$`proj:code` <- private$.code
      if (nzchar(private$.wkt2))
        attributes$`proj:wkt2` <- private$.wkt2
      if (nzchar(private$.projjson))
        attributes$`proj:projjson` <- private$.projjson
      attributes
    }
  ),
  active = list(
    #' @field code The "proj:code" attribute, a character string in
    #'   "authority:code" format identifying a CRS.
    code = function(value) {
      if (missing(value))
        private$.code
      else if (is.character(value) && length(value) == 1L &&
               grepl('[a-zA-Z0-9]+:[0-9]+', value))
        private$.code <- value
      else
        stop('`proj:code` must be character string in the right format', call. = FALSE)
    },

    #' @field wkt2 The "proj:wkt2" attribute, a character string giving a CRS in
    #'   WKT2 format.
    wkt2 = function(value) {
      if (missing(value))
        private$.wkt2
      else if (is.character(value) && length(value) == 1L)
        private$.wkt2 <- value
      else
        stop('`proj:wkt2` must be a character string with a WKT2 representation of a CRS', call. = FALSE)
    },

    #' @field projjson The "proj:projjson" attribute, a character string giving
    #'   a CRS in PROJJSON format.
    projjson = function(value) {
      if (missing(value))
        private$.projjson
      else if (is.character(value) && length(value) == 1L)
        private$.projjson <- value
      else
        stop('`proj:projjson` must be a character string with a PROJJSON representation of a CRS', call. = FALSE)
    }
  )
)
