#' Are indicated conventions supported?
#'
#' @description GeoZarr groups and arrays use Zarr conventions in their metadata
#'   to indicate the structure of their geospatial parameters. This function
#'   checks if the conventions listed in the metadata are supported by the
#'   current code base and returns the name of the principal convention that
#'   will supply axes and coordinates for the array.
#' @param metadata A `list` with the metadata of a group or array to test for
#'   supported conventions.
#' @return A character string giving the name of the principal convention for
#'   supplying axes and coordinates or `NA_character_` if no principal
#'   convention was found (for the "spatial" convention the parent group may
#'   supply the information). An error will be thrown if there are conventions
#'   listed that are not supported.
#' @noRd
.conventions_supported <- function(metadata) {
  listed <- metadata$attributes$zarr_conventions
  if (!length(listed)) return(NA_character_)

  avail <- GeoZarr.options$conventions

  # Discovery is by uuid or schema_url only; name is not a discovery field.
  # A convention is supported if at least one present discovery field matches.
  # If neither field is present the entry cannot be identified: unsupported.
  supported <- vapply(listed, function(cv) {
    if (!is.null(cv$uuid)       && cv$uuid       %in% avail$uuid)   return(TRUE)
    if (!is.null(cv$schema_url) && cv$schema_url %in% avail$schema) return(TRUE)
    FALSE
  }, FUN.VALUE = logical(1L), USE.NAMES = FALSE)

  listed_names <- vapply(listed, function(cv) cv$name %||% cv$uuid %||% '(unknown)',
                         FUN.VALUE = character(1L), USE.NAMES = FALSE)

  if (!all(supported))
    stop('Metadata lists unsupported conventions: ',
         paste(listed_names[!supported], collapse = ', '), call. = FALSE)

  if ('cs' %in% listed_names) 'cs'
  else if ('spatial' %in% listed_names) 'spatial'
  else NA_character_
}

#' GeoZarr package options
#'
#' Use this function to read or modify package options.
#'
#' @param key Character. A key whose value to modify. If missing, all options
#'   are returned.
#' @param value The new value for the option.
#' @return A list with all options if argument `key` is not provided,
#'   nothing otherwise.
#' @export
#' @examples
#' geozarr_options()
geozarr_options <- function(key, value) {
  if (missing(key))
    as.list(GeoZarr.options)
  else {
    switch(key,
           'max_explicit' = {
             if (is.numeric(value)) GeoZarr.options$max_explicit <- as.integer(value)
           })
  }
}

#' Make a data.frame slimmer by shortening long strings. List elements are
#' pasted together.
#' @param df A data.frame
#' @param width Maximum width of character entries. If entries are longer than
#' width - 3, they are truncated and then '...' added.
#' @return data.frame with slim columns
#' @noRd
.slim.data.frame <- function(df, width = 50L) {
  maxw <- width - 3L
  out <- as.data.frame(lapply(df, function(c) {
    if (is.list(c)) c <- sapply(c, paste0, collapse = ", ")
    if (!is.character(c)) c
    else
      sapply(c, function(e)
        if (nchar(e) > width) paste0(substr(e, 1, maxw), "...") else e
      )
  }))
  names(out) <- names(df)
  out
}

#' Test if vectors `x` and `y` have near-identical values.
#' @noRd
.near <- function(x, y) {
  abs(x - y) <= max(GeoZarr.options$eps * max(abs(x), abs(y)), 1e-12)
}

#' Test if vector `x` is monotonic, either increasing or decreasing. Return value
#' is -1L for monotonic decreasing, 0L for not monotonic, and 1L for monotonic
#' increasing.
#' @noRd
.monotonicity <- function(x) {
  if (!is.unsorted(x, na.rm = TRUE, strictly = TRUE)) 1L
  else if(!is.unsorted(-x, na.rm = TRUE, strictly = TRUE)) -1L
  else 0L
}

# There are formats out there that implicitly attach meaning to the name of an
# axis. This named vector makes an effort to find the axis abbreviation
# from the name. This is heuristics and a fallback option when no explicit
# attributes are found in the array metadata.
.common_axis_abbr <- c(
  # X - longitude / easting
  "x"                       = "X",
  "lon"                     = "X",
  "lons"                    = "X",
  "longitude"               = "X",
  "longitudes"              = "X",
  "long"                    = "X",
  "easting"                 = "X",
  "eastings"                = "X",
  "ni"                      = "X",  # NEMO ocean model
  "nj"                      = "Y",  # NEMO ocean model - keep together for context
  "ncol"                    = "X",
  "col"                     = "X",
  "column"                  = "X",
  "columns"                 = "X",
  "across_track"            = "X",
  "sample"                  = "X",  # satellite swath
  "samples"                 = "X",

  # Y - latitude / northing
  "y"                       = "Y",
  "lat"                     = "Y",
  "lats"                    = "Y",
  "latitude"                = "Y",
  "latitudes"               = "Y",
  "northing"                = "Y",
  "northings"               = "Y",
  "nrow"                    = "Y",
  "row"                     = "Y",
  "rows"                    = "Y",
  "along_track"             = "Y",  # satellite swath
  "line"                    = "Y",  # satellite swath
  "lines"                   = "Y",

  # Z - vertical
  "z"                       = "Z",
  "lev"                     = "Z",
  "level"                   = "Z",
  "levels"                  = "Z",
  "plev"                    = "Z",  # pressure level, CMIP convention
  "plevs"                   = "Z",
  "pressure"                = "Z",
  "depth"                   = "Z",
  "depths"                  = "Z",
  "height"                  = "Z",
  "heights"                 = "Z",
  "altitude"                = "Z",
  "elevation"               = "Z",
  "sigma"                   = "Z",  # sigma coordinate
  "eta"                     = "Z",  # ocean free-surface
  "layer"                   = "Z",
  "layers"                  = "Z",
  "soil_layer"              = "Z",
  "soil_layers"             = "Z",
  "sdepth"                  = "Z",  # soil depth

  # T - time
  "t"                       = "T",
  "time"                    = "T",
  "times"                   = "T",
  "step"                    = "T",  # forecast step
  "steps"                   = "T",
  "valid_time"              = "T",
  "forecast_time"           = "T",
  "lead_time"               = "T",
  "date"                    = "T",
  "datetime"                = "T",

  # Other - unstructured / ensemble / spectral / misc
  "cell"                    = "other",
  "cells"                   = "other",
  "node"                    = "other",
  "nodes"                   = "other",
  "face"                    = "other",
  "faces"                   = "other",
  "edge"                    = "other",
  "edges"                   = "other",
  "vertex"                  = "other",
  "vertices"                = "other",
  "member"                  = "other",  # ensemble
  "members"                 = "other",
  "ensemble"                = "other",
  "realization"             = "other",  # CMIP ensemble member
  "realizations"            = "other",
  "band"                    = "other",  # spectral / instrument band
  "bands"                   = "other",
  "channel"                 = "other",
  "channels"                = "other",
  "wavelength"              = "other",
  "wavenumber"              = "other",
  "spectral_band"           = "other",
  "category"                = "other",  # e.g. land cover class
  "type"                    = "other",
  "class"                   = "other",
  "record"                  = "other",  # generic record dimension
  "obs"                     = "other",  # observation
  "observation"             = "other",
  "trajectory"              = "other",
  "station"                 = "other",  # CF discrete sampling geometry
  "stations"                = "other",
  "profile"                 = "other",
  "profiles"                = "other",
  "scenario"                = "other"
)
