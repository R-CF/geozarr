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

  matched <- vapply(listed, .match_convention, character(1L), USE.NAMES = FALSE)
  if (anyNA(matched)) {
    labels <- vapply(listed[is.na(matched)],
                     function(cv) cv$name %||% cv$uuid %||% cv$schema_url %||% cv$spec_url %||% '(unknown)',
                     character(1L), USE.NAMES = FALSE)
    stop('Metadata lists unsupported conventions: ', paste(labels, collapse = ', '), call. = FALSE)
  }

  if ('cs' %in% matched) 'cs'
  else if ('spatial' %in% matched) 'spatial'
  else NA_character_
}

# The conventions table of package zarr has no `spec` column (yet).
.with_spec <- function(conv) {
  if (is.null(conv$spec)) conv$spec <- NA_character_
  conv
}

#' Split a convention URL into an identity and a version
#'
#' For GitHub URLs (raw.githubusercontent.com, and github.com with blob, tree
#' or raw) the identity is owner, repository and path: the git ref is replaced
#' by a placeholder, so tag and branch references to the same convention have
#' the same identity. For other hosts the last path segment of the form
#' `[v]MAJOR[.MINOR[.PATCH]]` is replaced. The version is that segment, or the
#' git ref if it has that form; a branch carries no version.
#' @param url Character string with a schema or spec URL.
#' @return A list with elements `base` (character) and `version` (integer
#'   vector of length 3, or `NULL` for an unversioned URL).
#' @noRd
.url_version <- function(url) {
  url <- sub("/refs/(tags|heads)/", "/", url)
  semver <- "^v?([0-9]+(\\.[0-9]+){0,2})$"
  gh <- regmatches(url, regexec("^(https://(raw\\.githubusercontent\\.com/[^/]+/[^/]+|github\\.com/[^/]+/[^/]+/(blob|tree|raw))/)([^/]+)(/.*)$", url))[[1L]]
  if (length(gh)) {
    base <- paste0(gh[2L], "{ref}", gh[6L])
    ref  <- gh[5L]
  } else {
    m <- regmatches(url, regexec("^(.*/)(v?[0-9]+(\\.[0-9]+){0,2})(/.*)$", url))[[1L]]
    if (!length(m))
      return(list(base = url, version = NULL))
    base <- paste0(m[2L], "{ref}", m[5L])
    ref  <- m[3L]
  }
  version <- if (grepl(semver, ref)) {
    v <- as.integer(strsplit(sub(semver, "\\1", ref), ".", fixed = TRUE)[[1L]])
    c(v, 0L, 0L)[1:3]
  }
  list(base = base, version = version)
}

#' Are two convention versions compatible?
#'
#' Semantic versioning: equal MAJOR versions are compatible, except under MAJOR
#' 0 where any MINOR change may break compatibility. An unversioned URL is
#' compatible with any version of the same identity.
#' @noRd
.versions_compatible <- function(a, b) {
  if (is.null(a) || is.null(b)) return(TRUE)
  a[1L] == b[1L] && (a[1L] > 0L || a[2L] == b[2L])
}

#' Identify a convention from its CMO
#'
#' The convention metadata object (CMO) is matched against the conventions
#' known to this package, in two steps. Identity: the `uuid` if present, which
#' is permanent across versions and locations; otherwise the identity of the
#' first URL present, `schema_url` before `spec_url` (see `.url_version()`).
#' `name` is not a discovery field. Version: the first URL in the CMO whose
#' version can be compared with that of the identified convention must be
#' compatible with it (see `.versions_compatible()`); without such a URL the
#' identity alone decides.
#' @param cmo A `list` with one entry of the "zarr_conventions" attribute.
#' @param known A `data.frame` with columns `name`, `uuid`, `schema` and `spec`.
#' @return The name of the matching known convention, or `NA_character_`.
#' @noRd
.match_convention <- function(cmo, known = GeoZarr.options$conventions) {
  urls <- Filter(Negate(is.null), list(schema = cmo$schema_url, spec = cmo$spec_url))
  parse <- function(u) if (is.na(u)) list(base = NA_character_, version = NULL) else .url_version(u)

  i <- if (!is.null(cmo$uuid))
    match(cmo$uuid, known$uuid)
  else if (length(urls)) {
    col <- names(urls)[1L]
    match(.url_version(urls[[1L]])$base,
          vapply(known[[col]], function(u) parse(u)$base, character(1L)))
  } else NA_integer_
  if (is.na(i)) return(NA_character_)

  for (col in names(urls)) {
    v <- .url_version(urls[[col]])$version
    k <- parse(known[[col]][i])$version
    if (!is.null(v) && !is.null(k))
      return(if (.versions_compatible(v, k)) known$name[i] else NA_character_)
  }
  known$name[i]
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
           'max_explicit' = if (is.numeric(value)) GeoZarr.options$max_explicit <- as.integer(value[1L]),
           'eps'          = if (is.numeric(value)) GeoZarr.options$eps <- value[1L]
    )
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

#' Test if vector `x` is regular, meaning the difference between successive
#' values is constant, possibly 0.
#' @noRd
.is_regular <- function(x, tolerance = GeoZarr.options$eps) {
  if (length(x) == 1L) return(FALSE)
  d <- diff(x)
  isTRUE(all.equal(d, rep(d[1L], length(d)), tolerance))
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

# This internal function takes details and a `name` for a new array to be
# created and the `location` being a group or location URI  where it needs to
# go. It returns a geozarr_array instance. The ... contain convention
# parameters.
.create_gza <- function(meta, cs, data, name = NULL, location, ...) {
  # Set the convention
  meta <- set_convention(meta, cs, ...)

  if (inherits(location, 'zarr_group')) {
    # Use a group in an existing zarr object
    if (!is.null(location$children[[name]]))
      stop('Group ', location$name, ' already has a node with name ', name, call. = FALSE)
    gza <- geozarr_array$new(name = name, metadata = meta, parent = location, coord_sys = cs)
  } else {
    # Create a new zarr object for the array
    z <- zarr::zarr$new(location)
    if (is.null(name) || !nzchar(name)) {
      # Single-array store
      gza <- geozarr_array$new(name = '', metadata = meta, parent = z, coord_sys = cs)
    } else {
      # Array in the root group
      zarr::zarr_group$new(name = '', parent = z)
      gza <- geozarr_array$new(name = name, metadata = meta, parent = z$root, coord_sys = cs)
     }
  }

  gza$write(data)
  gza$write_external_coordinates()
  gza
}
