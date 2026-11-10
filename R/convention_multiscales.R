#' GeoZarr "multiscales" convention
#'
#' @description This class implements the "multiscales" convention. The
#'   convention is placed in the attributes of a Zarr group and lists the nodes
#'   below that group that hold the same data at different resolutions, with for
#'   each such level the level that it was derived from.
#'
#'   The convention is registered via the following CMO:
#'
#' ```{r schema, eval = FALSE}
#' {
#'   "schema_url": "https://raw.githubusercontent.com/zarr-conventions/multiscales/refs/tags/v0.1/schema.json",
#'   "spec_url": "https://github.com/zarr-conventions/multiscales/blob/v0.1/README.md",
#'   "uuid": "d35379db-88df-4056-af3a-620245f8e347",
#'   "name": "multiscales",
#'   "description": "Multiscale layout of zarr datasets"
#' }
#' ```
#'
#'   The `multiscales` object has the following structure:
#'
#' ```json
#' {
#'   "multiscales": {
#'     "layout": [
#'       { "asset": "0",
#'         "spatial:shape": [1200, 1200],
#'         "spatial:transform": [10.0, 0.0, 500000.0, 0.0, -10.0, 5000000.0] },
#'       { "asset": "1",
#'         "derived_from": "0",
#'         "transform": { "scale": [2.0, 2.0], "translation": [0.0, 0.0] },
#'         "spatial:shape": [600, 600],
#'         "spatial:transform": [20.0, 0.0, 500000.0, 0.0, -20.0, 5000000.0] }
#'     ],
#'     "resampling_method": "average"
#'   }
#' }
#' ```
#'
#'   **Assets.** An `asset` is a path relative to the group holding the
#'   `multiscales` object. It may resolve to a group or to an array; the
#'   attributes do not tell which, that follows from the node itself.
#'
#'   **Scale.** The `scale` of a level is relative to the level named in its
#'   `derived_from`, not to the full-resolution level. The levels thus form a
#'   tree and the scale relative to the root of that tree is the product of the
#'   scales on the path to the root, see `cumulative_scale()`. A `transform` on
#'   a level without `derived_from` has no meaning and is ignored.
#'
#'   **What this class does not interpret.** The specification does not define
#'   to which dimensions the entries of `scale` and `translation` apply, nor in
#'   what order, nor the units of `translation`. This class therefore stores
#'   these properties but never derives georeferencing from them. The
#'   `spatial:transform` and `spatial:shape` of a level, when present, are the
#'   authoritative description of its grid. Levels added with `add_level()`
#'   always have a zero `translation`.
#'
#'   Registering this convention with `register()` also registers the "spatial"
#'   convention when any level has `spatial:` properties. Call `register()`
#'   after the levels have been set.
#'
#' @section Interpretation:
#'   Arrays in the levels of a multiscales group that uses the "spatial"
#'   convention are opened as `geozarr_array`. The specification is ambiguous or
#'   silent on several points; this implementation reads it as follows:
#'   * Interpretation:
#'     * An `asset` may be a group or an array; which one follows from the node, not from the attributes. An array takes part in a level when the "spatial" convention is declared on the multiscales group or on the array, and both names in `spatial:dimensions` are among its `dimension_names`. Other arrays in a level group, such as coordinate arrays, are left as plain Zarr arrays.
#'     * `scale` is relative to the level in `derived_from`, not to the full-resolution level. `cumulative_scale()` gives the product along `derived_from` to the root. A `transform` on a level without `derived_from` is ignored there.
#'     * `scale` and `translation` are never used for georeferencing, because the specification does not define the dimensions they apply to, their order, or the units of `translation`. The grid of a level comes from its `spatial:transform`.
#'     * `spatial:` and `proj:` attributes are taken, in order, from the array, its parent group, the layout object of the level, and the multiscales group. Attributes of the multiscales group thus reach arrays that are not its direct children, and a `spatial:transform` on the multiscales group never overrides that of a level. Arrays in a level need not declare a convention themselves.
#'     * The multiscales layout takes precedence over XArray-style coordinate arrays in a level group.
#'   * Stricter than the schema:
#'     * The layout is rejected when an asset is listed more than once, when a `derived_from` does not name an asset in the layout, when the `derived_from` references form a cycle, or when `scale` arrays differ in length. A rejected `multiscales` attribute is ignored with a warning; the store still opens.
#'     * A `scale` that is not the ratio of the cell sizes in the `spatial:transform` of a level and its source gives a warning on reading and an error in `add_level()`.
#'     * The convention must be declared in `zarr_conventions`. Stores citing the `v1` tags are not recognised.
#'     * Writing with `add_level()`:
#'     * A level must be added after its source. `scale` is required with `derived_from`, `translation` is always zero, and no `transform` is written for a level without `derived_from`.
#'     * Unknown properties of layout objects are preserved; other properties of the `multiscales` object are not.
#'   * Not supported:
#'     * Levels without their own `spatial:transform`. Such a level takes the `spatial:transform` of the multiscales group when there is one, which is wrong for all but the full-resolution level; otherwise opening the store fails.
#'     * A multiscales group without the "spatial" convention: the layout is available but the arrays are plain Zarr arrays.
#'     * A non-zero `translation`.
#' @docType class
#' @export
zarr_convention_multiscales <- R6::R6Class('zarr_convention_multiscales',
  inherit = zarr::zarr_convention,
  cloneable = FALSE,
  private = list(
    # Layout objects, one per level, in the order of the `layout` array.
    # Numeric arrays are held as atomic vectors.
    .layout = list(),

    # Optional: Default resampling method of the levels.
    .resampling_method = character(0),

    # Index into the layout of the source of each level, NA for a root.
    parents = function() {
      match(vapply(private$.layout, function(l) l$derived_from %||% NA_character_, character(1L)),
            self$assets)
    },

    # Assets whose `scale` is not the ratio of the cell sizes in the
    # `spatial:transform` of the level and of its source. The comparison is
    # sorted because the order of the entries of `scale` is not defined.
    conflicts = function() {
      lay <- private$.layout
      up  <- private$parents()
      bad <- vapply(seq_along(lay), function(i) {
        s <- lay[[i]]$transform$scale
        a <- lay[[i]]$`spatial:transform`
        b <- if (!is.na(up[i])) lay[[up[i]]]$`spatial:transform`
        length(s) == 2L && length(a) == 6L && length(b) == 6L &&
          !isTRUE(all.equal(sort(s), sort(abs(a[c(1L, 5L)] / b[c(1L, 5L)]))))
      }, logical(1L))
      self$assets[bad]
    }
  ),
  public = list(
    #' @description Create a new instance of a "multiscales" convention agent.
    #' @param multiscales Optional. A `multiscales` object as read from the
    #'   attributes of a Zarr group, in the form of a named `list` with element
    #'   "layout" and optionally "resampling_method".
    #' @details An error is thrown if argument `multiscales` is given but:
    #'   * it is not a list with a non-empty "layout" whose elements each have
    #'   an "asset" that is a relative path without ".." segments, the same
    #'   for "derived_from" when present;
    #'   * an asset is listed more than once;
    #'   * a "derived_from" does not name an asset in the layout, or the
    #'   "derived_from" references form a cycle;
    #'   * the "scale" arrays of the levels differ in length.
    #'
    #'   A warning is given for levels whose "scale" disagrees with the
    #'   "spatial:transform" of the level and its source. For those levels the
    #'   "spatial:transform" should be used.
    #' @return A new instance of a "multiscales" convention agent.
    initialize = function(multiscales) {
      super$initialize(
        name   = 'multiscales',
        schema = 'https://raw.githubusercontent.com/zarr-conventions/multiscales/refs/tags/v0.1/schema.json',
        uuid   = 'd35379db-88df-4056-af3a-620245f8e347'
      )
      private$.spec        <- 'https://github.com/zarr-conventions/multiscales/blob/v0.1/README.md'
      private$.description <- 'Multiscale layout of zarr datasets'

      if (!missing(multiscales)) {
        layout <- if (is.list(multiscales)) multiscales$layout
        ok <- is.list(layout) && length(layout) &&
              all(vapply(layout, function(l) is.list(l) && .ms_path(l$asset) &&
                                             (is.null(l$derived_from) || .ms_path(l$derived_from)),
                         logical(1L)))
        if (!ok)
          stop('Argument `multiscales` is misformed', call. = FALSE)

        private$.layout <- lapply(unname(layout), .ms_arrays, unlist)
        assets <- self$assets
        if (anyDuplicated(assets))
          stop('Layout lists an asset more than once', call. = FALSE)
        src <- vapply(layout, function(l) l$derived_from %||% NA_character_, character(1L))
        if (!all(src %in% c(assets, NA_character_)))
          stop('Layout has a "derived_from" that is not an asset in the layout', call. = FALSE)
        up <- p <- private$parents()
        for (i in seq_along(up)) p <- up[p]
        if (!all(is.na(p)))
          stop('Layout has "derived_from" references that form a cycle', call. = FALSE)
        n <- lengths(lapply(private$.layout, function(l) l$transform$scale))
        if (length(unique(n[n > 0L])) > 1L)
          stop('Layout has "scale" arrays of different lengths', call. = FALSE)

        if (!is.null(multiscales$resampling_method))
          self$resampling_method <- multiscales$resampling_method

        if (length(bad <- private$conflicts()))
          warning('"scale" disagrees with "spatial:transform" for multiscales level(s): ',
                  paste(bad, collapse = ', '), call. = FALSE)
      }
    },

    #' @description Register the use of this convention, and of the "spatial"
    #'   convention when any level has `spatial:` properties, in the attributes
    #'   of a Zarr group.
    #' @param attributes A `list` with Zarr attributes for a group.
    #' @param brief Logical flag to indicate if the registration should only
    #'   include the name and the schema URL or all details (default).
    #' @return The updated attributes.
    register = function(attributes, brief = FALSE) {
      attributes <- super$register(attributes, brief)
      if (any(vapply(private$.layout, function(l) any(startsWith(names(l), 'spatial:')), logical(1L))))
        attributes <- zarr_conv_spatial$new()$register(attributes, brief)
      attributes
    },

    #' @description Add a level to the layout. A level must be added after the
    #'   level that it is derived from, so the full-resolution level comes
    #'   first.
    #' @param asset Character string. Path to the group or array of the level,
    #'   relative to the group holding the `multiscales` object.
    #' @param derived_from Optional character string. The `asset` of the level
    #'   that the data of this level was resampled from. Omit for a level that
    #'   is not derived from another level.
    #' @param scale Numeric vector, required with `derived_from`. The ratio of
    #'   the cell size of this level to that of `derived_from`, one value per
    #'   scaled dimension.
    #' @param resampling_method Optional character string. The resampling
    #'   method of this level, when different from the default set with field
    #'   `resampling_method`.
    #' @param spatial_transform Optional numeric vector of 6 values, the
    #'   `spatial:transform` of this level. See [zarr_conv_spatial].
    #' @param spatial_shape Optional integer vector of 2 values, the
    #'   `spatial:shape` of this level.
    #' @details An error is thrown when:
    #'   * `asset` is not a relative path without ".." segments or is already
    #'   in the layout;
    #'   * `derived_from` is not an asset in the layout;
    #'   * `scale` is given without `derived_from`, is missing with
    #'   `derived_from`, is not positive and finite, or differs in length from
    #'   the `scale` of the other levels;
    #'   * `spatial_transform` is given for both this level and its source and
    #'   `scale` is not the ratio of their cell sizes.
    #' @return Self, invisibly.
    add_level = function(asset, derived_from = NULL, scale = NULL, resampling_method = NULL,
                         spatial_transform = NULL, spatial_shape = NULL) {
      assets <- self$assets
      if (!.ms_path(asset) || asset %in% assets)
        stop('Argument `asset` must be a relative path that is not yet in the layout', call. = FALSE)
      level <- list(asset = asset)

      if (is.null(derived_from)) {
        if (!is.null(scale))
          stop('Argument `scale` can only be given with `derived_from`', call. = FALSE)
      } else {
        if (!is.character(derived_from) || length(derived_from) != 1L || !derived_from %in% assets)
          stop('Argument `derived_from` must be an asset in the layout', call. = FALSE)
        n <- lengths(lapply(private$.layout, function(l) l$transform$scale))
        n <- n[n > 0L]
        if (!is.numeric(scale) || !length(scale) || !all(is.finite(scale)) || any(scale <= 0) ||
            (length(n) && length(scale) != n[1L]))
          stop('Argument `scale` must be a positive numeric vector of the same length as in other levels', call. = FALSE)
        level$derived_from <- derived_from
        level$transform <- list(scale = as.numeric(scale), translation = numeric(length(scale)))
      }

      if (!is.null(resampling_method)) {
        if (!is.character(resampling_method) || length(resampling_method) != 1L || !nzchar(resampling_method))
          stop('Argument `resampling_method` must be a character string', call. = FALSE)
        level$resampling_method <- resampling_method
      }
      if (!is.null(spatial_shape)) {
        if (!is.integer(spatial_shape) || length(spatial_shape) != 2L)
          stop('Argument `spatial_shape` must be an integer vector of length 2', call. = FALSE)
        level$`spatial:shape` <- spatial_shape
      }
      if (!is.null(spatial_transform)) {
        if (!is.numeric(spatial_transform) || length(spatial_transform) != 6L)
          stop('Argument `spatial_transform` must be a numeric vector of 6 values', call. = FALSE)
        level$`spatial:transform` <- spatial_transform
      }

      old <- private$.layout
      private$.layout <- c(old, list(level))
      if (asset %in% private$conflicts()) {
        private$.layout <- old
        stop('Argument `scale` is not the ratio of the cell sizes in `spatial_transform` of the level and its source', call. = FALSE)
      }
      invisible(self)
    },

    #' @description The scale of every level relative to the root of its
    #'   `derived_from` tree, being the product of the `scale` of the level
    #'   and of all levels between it and the root.
    #' @return A numeric matrix with a row for each level, named by asset, and
    #'   a column for each entry in `scale`. A root level has scale 1. A row is
    #'   `NA` when the level, or a level between it and the root, has no
    #'   `scale`.
    cumulative_scale = function() {
      lay <- private$.layout
      up  <- private$parents()
      sc  <- lapply(lay, function(l) l$transform$scale)
      n   <- max(0L, lengths(sc))
      sc  <- lapply(seq_along(lay), function(i)
               if (is.na(up[i])) rep(1, n) else sc[[i]] %||% rep(NA_real_, n))
      m   <- matrix(as.numeric(unlist(sc)), nrow = length(lay), byrow = TRUE,
                    dimnames = list(self$assets, NULL))

      out <- m
      p   <- up
      while (any(i <- !is.na(p))) {
        out[i, ] <- out[i, , drop = FALSE] * m[p[i], , drop = FALSE]
        p <- up[p]
      }
      out
    },

    #' @description Clear the levels that have been set.
    clear = function() {
      private$.layout <- list()
    },

    #' @description Retrieve the `multiscales` object as a list.
    #' @details An error is thrown when no levels have been set.
    #' @return A `list` with element "layout" and, when set,
    #'   "resampling_method".
    as_list = function() {
      if (!length(private$.layout))
        stop('At least one level must be set with `add_level()`', call. = FALSE)
      ms <- list(layout = lapply(private$.layout, .ms_arrays, as.list))
      if (length(private$.resampling_method))
        ms$resampling_method <- private$.resampling_method
      ms
    }
  ),
  active = list(
    #' @field assets (read-only) Character vector with the `asset` of every
    #'   level, in the order of the layout.
    assets = function(value) {
      if (missing(value))
        vapply(private$.layout, function(l) l$asset, character(1L))
      else
        stop("Can't set the assets, use `add_level()`", call. = FALSE)
    },

    #' @field layout (read-only) A `list` with for every level a list of its
    #'   properties. Properties of other conventions and unknown properties
    #'   are included as read.
    layout = function(value) {
      if (missing(value))
        private$.layout
      else
        stop("Can't set the layout, use `add_level()`", call. = FALSE)
    },

    #' @field resampling_method The default resampling method of the levels, a
    #'   character string.
    resampling_method = function(value) {
      if (missing(value))
        private$.resampling_method
      else if (is.character(value) && length(value) == 1L && nzchar(value))
        private$.resampling_method <- value
      else
        stop('`resampling_method` must be a character string', call. = FALSE)
    }
  )
)

# Is `p` a valid asset path: relative, without empty or ".." segments.
.ms_path <- function(p) {
  is.character(p) && length(p) == 1L &&
    grepl('^(?!/)(?!.*\\.\\.)[^/]+(/[^/]+)*$', p, perl = TRUE)
}

# Apply `f` to the numeric arrays of layout object `level`: `unlist` after
# reading from JSON, `as.list` before writing so that they are always
# serialised as a JSON array.
.ms_arrays <- function(level, f) {
  for (k in intersect(names(level), c('spatial:transform', 'spatial:shape')))
    level[[k]] <- f(level[[k]])
  for (k in intersect(names(level$transform), c('scale', 'translation')))
    level$transform[[k]] <- f(level$transform[[k]])
  level
}
