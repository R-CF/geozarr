# eopf.R - Driver for EOPF Sentinel products in Zarr v3 / GeoZarr format
#
# Requires package `zarr`. Load `geozarr` before opening a product to have the
# `spatial`, `proj` and `multiscales` conventions interpreted.
#
# The driver does three things and deliberately nothing more:
#   1. Build the hierarchy of a Zarr v3 HTTP store from its inline consolidated
#      metadata (generic, not EOPF-specific).
#   2. Expose the STAC assets embedded in the root group (EOPF profile).
#   3. Report where the product is inconsistent or non-conformant. Nothing is
#      repaired: a product that fails in `zarr` or `geozarr` should fail visibly.

# ------------------------------------------------------------------------------
# 1. Generic: Zarr v3 HTTP store with inline consolidated metadata
# ------------------------------------------------------------------------------

#' HTTP store reading the hierarchy from Zarr v3 inline consolidated metadata
#'
#' A `zarr_httpstore` can only see the node at its root because HTTP has no
#' listing. This descendant uses the `consolidated_metadata` member of the root
#' group metadata, as written by zarr-python, to enumerate and describe all
#' nodes in the store.
zarr_httpstore_consolidated <- R6::R6Class('zarr_httpstore_consolidated',
  inherit = zarr::zarr_httpstore,
  cloneable = FALSE,
  private = list(
    .cm = list()   # Metadata documents of all non-root nodes, by node key
  ),
  public = list(
    #' @param url The URL of the root of the Zarr store.
    #' @details An error is raised when the root metadata has no
    #'   `consolidated_metadata` member, or when its `kind` is not "inline".
    initialize = function(url) {
      super$initialize(url)

      cm <- private$.metadata$consolidated_metadata
      if (is.null(cm))
        stop('No consolidated metadata in the root of the store', call. = FALSE)
      if (!identical(cm$kind, 'inline'))
        stop('Unsupported kind of consolidated metadata: ', cm$kind %||% '(null)', call. = FALSE)

      private$.cm <- cm$metadata
      private$.nodes <- names(cm$metadata)
      private$.metadata$consolidated_metadata <- NULL  # Inventory, not root group metadata
    },

    exists = function(key) {
      key == '/' || key %in% private$.nodes
    },

    get_metadata = function(prefix) {
      if (prefix %in% c('/', '')) private$.metadata
      else private$.cm[[sub('/$', '', prefix)]]
    }
  ),
  active = list(
    #' @field nodes (read-only) The paths of all non-root nodes listed in the
    #'   consolidated metadata.
    nodes = function(value) {
      if (missing(value))
        paste0('/', private$.nodes)
    }
  )
)

# ------------------------------------------------------------------------------
# 2. EOPF profile
# ------------------------------------------------------------------------------

#' Open an EOPF product
#'
#' @param url The URL of the root of the EOPF Zarr store.
#' @return A `zarr` object.
#' @details An error is raised when the root group has no `stac_discovery`
#'   attribute, which is what identifies an EOPF product.
eopf_open <- function(url) {
  z <- zarr::zarr$new(zarr_httpstore_consolidated$new(url))
  if (is.null(z$root$attributes$stac_discovery))
    stop('Not an EOPF product: no "stac_discovery" attribute in the root group', call. = FALSE)
  z
}

#' The STAC assets of an EOPF product
#'
#' EOPF profile: an asset `href` is the path of a Zarr node from the root of the
#' store, not a URL reference.
#' @param z A `zarr` object from `eopf_open()`.
#' @return A `data.frame` with one row per asset: its `key`, node `path`,
#'   `title`, media `type`, `roles`, the `node` type found at the path (`NA` if
#'   there is no such node) and the asset statistics.
eopf_assets <- function(z) {
  assets <- z$root$attributes$stac_discovery$assets
  stats <- c('minimum', 'maximum', 'mean', 'stddev', 'count', 'valid_percent')
  rows <- lapply(assets, function(a) {
    node <- z$get_node(a$href)
    c(list(path  = a$href,
           title = a$title %||% NA_character_,
           type  = a$type %||% NA_character_,
           roles = paste(a$roles, collapse = ','),
           node  = if (is.null(node)) NA_character_
                   else if (inherits(node, 'zarr_group')) 'group' else 'array'),
      lapply(setNames(stats, stats), function(s) a$statistics[[s]] %||% NA_real_))
  })
  data.frame(key = names(assets), do.call(rbind.data.frame, rows), row.names = NULL)
}

# ------------------------------------------------------------------------------
# 3. Checks
# ------------------------------------------------------------------------------

#' Read a single element from every array
#'
#' This exercises the full read path (store, sharding, codecs, data type) of
#' `zarr` and, for arrays managed by `geozarr`, the construction of the
#' coordinate system.
#' @param z A `zarr` object from `eopf_open()`.
#' @param paths Paths of the arrays to test. Default is all arrays.
#' @return A `data.frame` with one row per array: its `path`, R6 `class`, and
#'   the error message from reading (`read`) and from building the coordinate
#'   system (`cs`), `NA` when successful or not applicable.
eopf_read_test <- function(z, paths = z$arrays) {
  msg <- function(expr) tryCatch({expr; NA_character_}, error = conditionMessage)
  rows <- lapply(paths, function(p) {
    arr <- z$get_node(p)
    data.frame(path  = p,
               class = class(arr)[1L],
               read  = msg(arr$read(lapply(arr$shape, function(d) c(1L, 1L)))),
               cs    = if (inherits(arr, 'geozarr_array')) msg(arr$coordinate_system)
                       else NA_character_)
  })
  do.call(rbind, rows)
}

#' Check an EOPF product for inconsistencies and non-conformance
#'
#' @param z A `zarr` object from `eopf_open()`.
#' @param data Logical. If `TRUE` (default), the first values of the X and Y
#'   coordinate arrays are read to compare against the `spatial:transform`.
#' @return A `data.frame` of findings with columns `check`, `path` and `detail`.
#'   Findings that affect many nodes alike are reported once, with a count and
#'   the first path.
eopf_check <- function(z, data = TRUE) {
  found <- list()
  add <- function(check, path, detail)
    found[[length(found) + 1L]] <<- data.frame(check = check, path = path, detail = detail)
  # Report a finding once for all paths where it applies
  add_n <- function(check, paths, detail)
    if (length(paths))
      add(check, paths[1L], paste0(length(paths), ' node(s): ', detail))

  groups <- lapply(setNames(nm = z$groups), z$get_node)
  arrays <- lapply(setNames(nm = z$arrays), z$get_node)
  # Paths of the nodes for which predicate `f` holds, or which have attribute `f`
  pick <- function(nodes, f) {
    if (is.character(f)) {
      name <- f
      f <- function(n) !is.null(n$attributes[[name]])
    }
    names(nodes)[vapply(nodes, function(n) isTRUE(f(n)), logical(1L))]
  }
  root <- z$root$attributes
  near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b)))

  # --- STAC --------------------------------------------------------------------
  stac   <- root$stac_discovery
  assets <- eopf_assets(z)

  for (l in stac$links)
    if (identical(l$rel, 'self') && is.null(z$store$get(sub('^\\./', '', l$href))))
      add('stac-self-link', '/', paste('"self" link does not resolve:', l$href))

  add_n('asset-href-absolute', assets$path[startsWith(assets$path, '/')],
        'href is an absolute-path reference; under RFC 3986 it resolves to the host root, not the store')
  for (i in which(is.na(assets$node)))
    add('asset-unresolved', assets$path[i], paste('No node for asset', assets$key[i]))
  add_n('asset-untyped', assets$path[is.na(assets$type) | !nzchar(assets$roles)],
        'asset has no "type" and/or "roles"')

  packed <- intersect(assets$path[!is.na(assets$minimum)],
                      union(pick(arrays, 'scale_factor'), pick(arrays, 'add_offset')))
  add_n('asset-statistics-packed', packed,
        'statistics are on stored values but the array is packed; the asset carries no scale/offset')

  covered <- assets$path[assets$node %in% 'array']
  grp <- assets$path[assets$node %in% 'group']
  bare <- setdiff(names(arrays), covered)
  bare <- bare[!Reduce(`|`, lapply(grp, function(g) startsWith(bare, paste0(g, '/'))), logical(length(bare)))]
  add_n('asset-coverage', bare,
        paste('arrays not reachable from any asset:',
              paste(names(table(basename(bare))), collapse = ', ')))

  # --- Zarr --------------------------------------------------------------------
  add_n('zarr-nested-consolidated',
        pick(groups[names(groups) != '/'], function(g) !is.null(g$metadata$consolidated_metadata)),
        'non-root group carries its own "consolidated_metadata"')

  # Nodes in the consolidated metadata that `zarr` did not build
  for (p in setdiff(z$store$nodes, c(names(groups), names(arrays)))) {
    dt <- z$store$get_metadata(paste0(substring(p, 2L), '/'))$data_type
    add('zarr-node-not-built', p,
        if (is.list(dt)) paste0('Data type "', dt$name, '" is not in the Zarr v3 core specification')
        else 'Node is in the consolidated metadata but not in the hierarchy')
  }

  add_n('zarr-degenerate-shard',
        pick(arrays, function(a) {
          cdc <- a$metadata$codecs[[1L]]
          identical(cdc$name, 'sharding_indexed') &&
            all(unlist(cdc$configuration$chunk_shape) >= unlist(a$metadata$shape))
        }),
        'sharded array fits in a single inner chunk')

  # --- CF attributes -----------------------------------------------------------
  if (is.null(root$Conventions))
    add_n('cf-undeclared', pick(arrays, 'grid_mapping'),
          'CF attributes are used but the root group has no "Conventions" attribute')

  add_n('cf-grid-mapping-on-group', pick(groups, 'grid_mapping'),
        '"grid_mapping" is a CF attribute of data variables, not of groups')

  add_n('cf-fillvalue-encoded',
        pick(arrays, function(a)
          is.character(a$attributes$`_FillValue`) && !is.list(a$metadata$data_type)),
        '"_FillValue" of a numeric array is a string (base64 encoded)')

  add_n('cf-wkt1', pick(arrays, function(a) grepl('^(PROJCS|GEOGCS)\\[', a$attributes$crs_wkt)),
        '"crs_wkt" is WKT 1; CF specifies OGC CRS WKT (WKT 2)')

  add_n('cf-flag-meanings-list', pick(arrays, function(a) length(a$attributes$flag_meanings) > 1L),
        '"flag_meanings" is a list; CF specifies a single blank-separated string')

  add_n('cf-valid-range',
        pick(arrays, function(a) {
          dt <- a$metadata$data_type
          is.character(dt) && grepl('^uint[0-9]+$', dt) &&
            a$attributes$valid_max > 2^as.integer(sub('uint', '', dt)) - 1
        }),
        '"valid_max" exceeds the range of the data type')

  # --- spatial / proj / multiscales --------------------------------------------
  # Group-level `spatial` and `proj` attributes apply to direct child arrays only
  inert <- pick(groups, function(g)
    (!is.null(g$attributes$`spatial:bbox`) || !is.null(g$attributes$`proj:code`)) &&
      !any(vapply(g$children, inherits, logical(1L), 'zarr_array')))
  add_n('spatial-no-arrays', inert,
        'group has "spatial" or "proj" attributes but no child arrays for them to apply to')

  for (p in pick(groups, 'spatial:shape')) {
    g <- groups[[p]]
    a <- g$attributes
    tr <- a$`spatial:transform`

    # "spatial:shape" is [height, width], like "spatial:dimensions" is (y, x).
    # With "pixel" registration (the default) the transform origin and the bbox
    # are on the outer cell edges; with "node" registration they are on the
    # centres of the outer cells, so the extent is one cell less.
    node <- identical(a$`spatial:registration`, 'node')
    n <- setNames(a$`spatial:shape`, a$`spatial:dimensions`)
    dims <- a$`spatial:dimensions`
    xdim <- dims[2L]; ydim <- dims[1L]
    xr <- range(tr[3L], tr[3L] + (n[[xdim]] - node) * tr[1L])
    yr <- range(tr[6L], tr[6L] + (n[[ydim]] - node) * tr[5L])
    if (!near(a$`spatial:bbox`, c(xr[1L], yr[1L], xr[2L], yr[2L])))
      add('spatial-bbox', p, paste0('bbox from transform and shape with "', if (node) 'node' else 'pixel',
                                    '" registration is ', paste(c(xr[1L], yr[1L], xr[2L], yr[2L]), collapse = ', ')))

    for (ch in g$children)
      if (inherits(ch, 'zarr_array') && identical(unlist(ch$metadata$dimension_names), dims) &&
          !near(ch$shape, a$`spatial:shape`))
        add('spatial-shape', ch$path, paste('Array shape', paste(ch$shape, collapse = ' x '),
                                            'differs from "spatial:shape" of its group'))

    # proj:code against the CF grid mapping
    gm <- if (!is.null(a$grid_mapping)) g$children[[a$grid_mapping]]
    if (!is.null(gm)) {
      wkt <- gm$attributes$crs_wkt %||% ''
      code <- sub('.*AUTHORITY\\["([A-Za-z]+)","([0-9]+)"\\]\\]$', '\\1:\\2', wkt)
      if (!identical(toupper(code), toupper(a$`proj:code`)))
        add('crs-mismatch', p, paste0('"proj:code" is ', a$`proj:code`,
                                      ', grid mapping "', a$grid_mapping, '" says ', substr(code, 1L, 40L)))
    }

    # Coordinate arrays against the transform
    if (data) for (d in c(xdim, ydim)) {
      crd <- g$children[[d]]
      if (is.null(crd)) next
      i <- if (d == xdim) c(3L, 1L) else c(6L, 5L)
      v <- tryCatch(as.numeric(crd$read(list(c(1L, 1L)))), error = function(e) NA_real_)
      centre <- tr[i[1L]] + if (node) 0 else tr[i[2L]] / 2
      if (!near(v, centre))
        add('spatial-coordinates', crd$path,
            paste0('First coordinate is ', format(v, digits = 12L), '; cell centre from transform with "',
                   if (node) 'node' else 'pixel', '" registration is ', format(centre, digits = 12L)))
    }
  }

  for (p in pick(groups, 'multiscales')) {
    g <- groups[[p]]
    for (lvl in g$attributes$multiscales$layout) {
      child <- g$children[[lvl$asset]]
      if (is.null(child)) {
        add('multiscales-level', p, paste('No group for level', lvl$asset))
        next
      }
      for (nm in c('spatial:shape', 'spatial:transform'))
        if (!is.null(lvl[[nm]]) && !near(lvl[[nm]], child$attributes[[nm]]))
          add('multiscales-mismatch', child$path,
              paste0('"', nm, '" in the layout differs from that of the level group'))
    }
  }

  if (length(found)) do.call(rbind, found)
  else data.frame(check = character(0), path = character(0), detail = character(0))
}
