#' GeoZarr Array
#'
#' @description This class implements a GeoZarr array. A GeoZarr array is like a
#'   regular Zarr array but it has attributes and/or associated Zarr arrays that
#'   provide a coordinate system for the array.
#' @docType class
#' @export
geozarr_array <- R6::R6Class('geozarr_array',
  inherit = zarr::zarr_array,
  cloneable = FALSE,
  private = list(
    # The coordinate system of this array
    .cs = NULL,

    # Print GeoZarr details as part of printing a group. The withCLI argument
    # indicates if package cli should be used.
    print_details = function(withCLI = TRUE) {
      thm <- zarr::zarr_options('zarr_theme')
      if (is.list(thm)) {
        this_div <- cli::cli_div(theme = thm)
        cli::cli_rule('Coordinate system')
        cli::cli_end(this_div)
      } else
        cat('\nCoordinate system:\n')
      private$.cs$print_axes()
    },

    # Remove domain attributes prior to printing this geozarr_array
    display_attributes = function() {
      atts <- private$.metadata[['attributes']]
      if (length(atts)) {
        nms <- names(atts)
        atts[!(startsWith(nms, 'spatial:') | (nms %in% c('zarr_conventions', 'cs', 'geolocation')))]
      } else list()
    },

    # This function resolves a `ref` element in the array attributes. Argument
    # `node` is a path (character string) from this array to the desired node,
    # required. The path is relative from this array if argument `uri` is not
    # provided, or an absolute path from the root of the `uri` store otherwise.
    # Arguments `attribute` and `uri` are optional. The return value is a JSON
    # schema in the form of a list (possibly a scalar value) if argument
    # `attribute` is provided, or a `zarr_array` if not. If the arguments do not
    # point to anything an error is thrown.
    resolve_external_node = function(node, attribute, uri) {
      if (missing(uri) || is.null(uri) || !nzchar(uri)) {
        # Local reference: Resolve the path relative to this array
        parts <- strsplit(node, split = '/', fixed = TRUE)[[1L]]
        if (!(len <- length(parts)) || startsWith(node, '/'))
          stop('Argument `node` must be a relative path from the referring array or group', call. = FALSE)
        referred_node <- self
        ndx <- 1L
        while (ndx <= len && !is.null(referred_node)) {
          referred_node <- if (parts[ndx] == '..') referred_node$parent
                           else referred_node$children[[parts[ndx]]]
          ndx <- ndx + 1L
        }
      } else {
        # External store: Load the store and grab the node indicated
        stop('Not yet supported', call. = FALSE)
      }

      if (missing(attribute) || is.null(attribute))
        referred_node
      else if (!is.null(referred_node))
        .json_pointer(referred_node$metadata, attribute)
    },

    # The CRS objects of the "cs" attribute in metadata `meta`. A CRS object
    # referenced in a group is resolved and the references in it are rebased
    # to this array. A geolocation object in a CRS object is kept only if the
    # node that defines the CRS object declares the geolocation convention.
    cs_crs = function(meta) {
      array_geo <- .declares(meta, 'geolocation')
      lapply(meta$attributes$cs$crs, function(crs) {
        ref <- crs$ref
        if (is.null(ref)) {
          if (!array_geo) crs$geolocation <- NULL
          return(crs)
        }
        if (!is.null(ref$uri))
          stop('CRS references to an external store are not yet supported', call. = FALSE)
        grp <- private$resolve_external_node(node = ref$node)
        resolved <- if (!is.null(grp) && !is.null(ref$attribute))
          .json_pointer(grp$metadata, ref$attribute)
        if (!is.list(resolved) || is.null(resolved$axes))
          stop('CRS reference "', ref$node, '" with attribute "', ref$attribute %||% '',
               '" does not resolve to a CRS object', call. = FALSE)
        if (!.declares(grp$metadata, 'geolocation')) resolved$geolocation <- NULL
        .rebase_refs(resolved, ref$node)
      })
    },

    # Create a coordinate system using the cs convention.
    build_cs = function(meta) {
      atts            <- meta$attributes
      dimension_names <- meta$dimension_names
      shape           <- meta$shape

      cs <- atts$cs
      if (is.null(cs))
        stop('Required attribute "cs" not found in array metadata', call. = FALSE)
      if (!length(cs$crs))
        stop('Attribute "cs" must contain at least one CRS object', call. = FALSE)
      crs_list <- private$cs_crs(meta)

      # Collect all axis definitions from all CRS objects, keyed by dimension
      # name. Later CRS objects win on name collision (should not occur in a
      # valid store, but we need a deterministic rule).
      all_axes <- list()
      for (crs in crs_list) {
        if (!is.null(crs$axes))
          all_axes[names(crs$axes)] <- crs$axes
      }

      # Internal function: Build one CoordinateSystemAxis from an axis definition.
      # dim_length is the number of elements along this dimension in the array;
      # pass 1L for scalar axes not present in dimension_names.
      build_one_axis <- function(dim_name, ax_def, dim_length, attributes = NULL) {
        if (is.null(ax_def))
          return(private$cs_ordinal_axis(dim_name, dim_length))

        abbr <- ax_def$abbreviation %||% ''
        direction <- toupper(ax_def$direction %||% 'OTHER')
        if (!direction %in% AxisDirection) direction <- 'OTHER'

        coord_defs <- ax_def$coordinates
        if (!is.list(coord_defs) || !length(coord_defs))
          return(private$cs_ordinal_axis(dim_name, dim_length))

        coords_list <- lapply(seq_along(coord_defs), function(j) {
          private$cs_build_coordinates(coord_defs[[j]], dim_name, j, dim_length, direction, attributes)
        })
        names(coords_list) <- vapply(coords_list, function(cd) cd$name, character(1L))

        CoordinateSystemAxis$new(name = dim_name, abbreviation = abbr, coordinates = coords_list)
      }

      # 1. Dimensional axes, in dimension_names order.
      axes <- lapply(seq_along(dimension_names), function(i) {
        build_one_axis(dimension_names[i], all_axes[[dimension_names[i]]], shape[i],
                       all_axes[[dimension_names[i]]]$attributes)
      })
      names(axes) <- dimension_names

      # 2. Scalar axes: present in cs metadata but absent from dimension_names.
      #    These describe coordinates of degenerate (length-1) dimensions that
      #    have been excluded from the array shape. Direction cannot be
      #    determined from a single value, so we force 'OTHER'. Order follows
      #    the order the names appear across the CRS objects (already preserved
      #    in all_axes since R lists maintain insertion order).
      scalar_names <- setdiff(names(all_axes), dimension_names)
      if (length(scalar_names)) {
        scalar_axes <- lapply(scalar_names, function(nm) {
          ax <- build_one_axis(nm, all_axes[[nm]], 1L)
          ax$coordinates$direction <- 'OTHER'
          ax
        })
        names(scalar_axes) <- scalar_names
        axes <- c(axes, scalar_axes)
      }

      # 3. Geolocation, possibly attached to any of the CRS objects
      geo <- do.call(c, lapply(crs_list, function(crs) crs$geolocation))
      if (anyDuplicated(names(geo)))
        stop('Attribute "cs" has more than one "', names(geo)[anyDuplicated(names(geo))],
             '" geolocation', call. = FALSE)

      private$attach_geolocation(CoordinateSystem$new(name = cs$name %||% 'cs', axes = axes), geo)
    },

    # Set the geolocation `geo`, a `geolocation` object read from metadata, on
    # coordinate system `cs`, if there is one. Returns `cs`.
    attach_geolocation = function(cs, geo) {
      if (length(geo))
        cs$geolocation <- zarr_convention_geolocation$new(geo)
      cs
    },

    # Build a Coordinates instance from one element of an axis's `coordinates`
    # array in the cs attribute.
    cs_build_coordinates = function(coord_def, dim_name, index, dim_length, direction, attributes = NULL) {
      # Coordinate set name: use declared name or synthesise one
      crd_name <- coord_def$name %||% paste0(dim_name, '_coordinates', if (index > 1L) index else '')
      cv <- private$cs_build_values(coord_def$values, dim_name, dim_length)

      bounds <- if (!is.null(coord_def$boundaries))
        private$cs_build_bounds(coord_def$boundaries, dim_name)
      else
        NULL

      time <- coord_def$time
      if (is.null(time)) {
        if (!is.null(cv$length))
          CoordinatesPacked$new(name = crd_name, direction = direction,
                                unit = coord_def$unit %||% '', values = cv$values,
                                length = cv$length, bounds = bounds,
                                attributes = attributes)
        else
          CoordinatesIrregular$new(name = crd_name, direction = direction,
                                 unit = coord_def$unit %||% '', values = cv$values,
                                 bounds = bounds, attributes = attributes)
      } else {
        values <- cv$values
        if (!is.null(cv$length))
          values <- seq(from = values[1L], by = values[2L], length.out = cv$length)
        CoordinatesTime$new(name = crd_name, direction = direction, unit = time$unit,
                            epoch = time$epoch, calendar = time$calendar,
                            values = values, bounds = bounds,
                            attributes = attributes)
      }
    },

    # Resolve a `values` object into an instance inheriting from CoordinateValues
    cs_build_values = function(values_def, dim_name, dim_length) {
      if (is.null(values_def))
        stop('Axis "', dim_name, '": coordinate set is missing required "values" element', call. = FALSE)

      if (!is.null(values_def$regular)) {
        rv <- unlist(values_def$regular)
        if (length(rv) != 2L)
          stop('Axis "', dim_name, '": "values.regular" must have exactly 2 elements', call. = FALSE)
        return(list(mode = storage.mode(rv), values = rv, length = dim_length))
      }

      if (!is.null(values_def$explicit)) {
        v <- unlist(values_def$explicit)
        return(.make_coordinate_values(v))
      }

      if (!is.null(values_def$external)) {
        raw <- private$cs_load_external(values_def$external, dim_name, 'values')
        return(.make_coordinate_values(raw))
      }

      stop('Axis "', dim_name, '": "values" must have one of "regular", "explicit", or "external".', call. = FALSE)
    },

    # Resolve a `boundaries` object into the bounds representation expected by
    # Coordinates$new():
    #   regular  -> numeric vector c(below, above)
    #   external -> 2-row numeric matrix (row 1 = lower bound, row 2 = upper bound)
    cs_build_bounds = function(bounds_def, dim_name) {
      if (!is.null(bounds_def$regular)) {
        bv <- unlist(bounds_def$regular)
        if (length(bv) != 2L)
          stop('Axis "', dim_name, '": boundaries must have exactly 2 elements', call. = FALSE)
        return(bv)
      }

      if (!is.null(bounds_def$external)) {
        raw <- private$cs_load_external(bounds_def$external, dim_name, 'boundaries')
        if (!is.matrix(raw) || nrow(raw) != 2L)
          stop('Axis "', dim_name, '": external boundaries array must be a 2-row matrix', call. = FALSE)
        return(raw)
      }

      stop('Axis "', dim_name, '": boundaries must have one of "regular" or "external"', call. = FALSE)
    },

    # Load data from an `external` reference: { ref: { node, uri? } }.
    cs_load_external = function(external_def, dim_name, context) {
      ref <- external_def$ref
      if (is.null(ref) || is.null(ref$node))
        stop('Axis "', dim_name, '": ', context, ' external reference is missing the required "node" field', call. = FALSE)
      if (!is.null(ref$attribute))
        stop('Axis "', dim_name, '": ', context, ' uses an attribute reference which is not supported', call. = FALSE)

      ref_arr <- private$resolve_external_node(node = ref$node, uri = ref$uri)
      if (is.null(ref_arr))
        warning('External node "', ref$node, '" not found!')
      else
        ref_arr$read()
    },

    # Fallback: a 0-based ordinal axis for dimensions carrying no cs metadata.
    cs_ordinal_axis = function(dim_name, dim_length) {
      crd_name <- paste0(dim_name, '_coordinates')
      coords   <- CoordinatesOrdinal$new(name = crd_name, direction = 'OTHER', length = dim_length)
      CoordinateSystemAxis$new(name = dim_name, abbreviation = '', coordinates = setNames(list(coords), crd_name))
    },

    # Create a coordinate system using the spatial convention
    build_spatial = function(meta) {
      # Get spatial attributes from the parent group and merge with local attributes
      parent_atts <- if (is.null(private$.parent)) NULL else private$.parent$metadata$attributes
      atts <- meta$attributes

      # Extract the parameters and check their validity
      dimension_names <- meta$dimension_names
      if (is.null(dimension_names))
        stop('Required top-level metadata item "dimension_names" not found', call. = FALSE)

      dimensions <- atts$`spatial:dimensions` %||% parent_atts$`spatial:dimensions` # Y,X order
      if (is.null(dimensions))
        stop('Required attribute "spatial:dimensions" not found in metadata', call. = FALSE)
      dims <- length(dimensions)
      if (dims != 2L)
        stop('Attribute "spatial:dimensions" has wrong number of items', call. = FALSE)

      dim_order <- match(dimensions, dimension_names)
      if (any(is.na(dim_order)))
        stop('Attribute "spatial:dimensions" has names not present in the "dimension_names" metadata of the array', call. = FALSE)

      transform_type <- atts$`spatial:transform_type` %||% parent_atts$`spatial:transform_type` %||% 'affine'
      if (transform_type != 'affine')
        stop('Only "affine" transformation supported', call. = FALSE)

      transform <- atts$`spatial:transform` %||% parent_atts$`spatial:transform`
      if (is.null(transform))
        stop('Required attribute "spatial:transform" not found in metadata', call. = FALSE)
      if (length(transform) != 6L)
        stop('Attribute "spatial:transform" has wrong number of items', call. = FALSE)

      bbox <- atts$`spatial:bbox` %||% parent_atts$`spatial:bbox`
      if (!is.null(bbox) && length(bbox) != 4L)
        stop('Attribute "spatial:bbox" has wrong number of items', call. = FALSE)

      shape <- atts$`spatial:shape` %||% parent_atts$`spatial:shape`
      if (!is.null(shape) && length(shape) != dims)
        stop('Attribute "spatial:shape" has wrong number of items', call. = FALSE)

      registration <- atts$`spatial:registration` %||% parent_atts$`spatial:registration` %||% 'pixel'
      if (!(registration %in% c('pixel', 'node')))
        stop('Attribute "spatial:registration" has bad value', call. = FALSE)

      # Build the coordinate system
      # X and Y coordinates are always numeric and always present
      half <- if (registration == 'pixel') 0.5 else 0
      coords <- CoordinatesPacked$new(name = paste0(dimensions[2L], '_coordinates'), direction = 'EAST',
                                      unit = '', values = c(transform[3L] + transform[1L] * half, transform[1L]),
                                      length = meta$shape[dim_order[2L]],
                                      bounds = c(-0.5, 0.5) * transform[1L])
      X_axis <- CoordinateSystemAxis$new(name = dimensions[2L], abbreviation = 'X',
                                         coordinates = list(X_coordinates = coords))

      coords <- CoordinatesPacked$new(name = paste0(dimensions[1L], '_coordinates'), direction = 'NORTH',
                                      unit = '', values = c(transform[6L] + transform[5L] * half, transform[5L]),
                                      length = meta$shape[dim_order[1L]],
                                      bounds = c(-0.5, 0.5) * abs(transform[5L]))
      Y_axis <- CoordinateSystemAxis$new(name = dimensions[1L], abbreviation = 'Y',
                                         coordinates = list(Y_coordinates = coords))

      # Add any other axes, if present. These will all be ordinal axes as there
      # are no coordinates available in the metadata or attributes.
      axes <- lapply(seq_along(dimension_names), function(ndx) {
        name <- dimension_names[ndx]
        if (ndx == dim_order[2L]) X_axis
        else if (ndx == dim_order[1L]) Y_axis
        else {
          coords <- CoordinatesOrdinal$new(name = paste0(name, '_coordinates'), direction = 'OTHER',
                                           length = meta$shape[ndx])
          CoordinateSystemAxis$new(name = name, abbreviation = '',
                                   coordinates = setNames(list(coords), paste0(name, '_coordinates')))
        }
      })
      names(axes) <- dimension_names

      private$attach_geolocation(CoordinateSystem$new(name = 'local_CS', axes = axes),
                                 if (.declares(meta, 'geolocation')) atts$geolocation)
    },

    # Create a coordinate system from sibling "coordinate variable" arrays.
    build_xarray = function(meta) {
      # Internal helper function
      .make_axis <- function(name, abbreviation, direction, units, values) {
        crd_name <- paste0(name, '_coordinates')
        coords <- if (missing(values) || all(is.na(values)))
          CoordinatesOrdinal$new(name = crd_name, direction = 'OTHER', shape[i])
        else {
          vals <- .make_coordinate_values(values)
          if (is.null(vals$length))
            CoordinatesIrregular$new(name = crd_name, direction = direction,
                                     unit = units, values = vals$values, bounds = NULL)
          else
            CoordinatesPacked$new(name = crd_name, direction = direction,
                                  unit = units, values = vals$values,
                                  length = vals$length, bounds = NULL)
        }
        CoordinateSystemAxis$new(name = name, abbreviation = abbreviation,
                                 coordinates = stats::setNames(list(coords), crd_name))
      }

      shape <- meta$shape
      dim_names <- meta$dimension_names %||% meta$attributes$`_ARRAY_DIMENSIONS` # Guaranteed to have one or the other
      # coordinates <- meta$attributes$coordinates # May be NULL, probably not needed because of dim_names anyway
      axes <- vector('list', length = length(shape))
      for (i in seq_along(shape)) { # Assuming dim_names and shape have the same length, as they should
        dim_prefix <- paste0(private$.parent$prefix, dim_names[i], "/")
        dim_meta   <- private$.store$get_metadata(dim_prefix)
        if (!is.null(dim_meta) && dim_meta$node_type == "array") {
          dim_array <- zarr::zarr_array$new(dim_names[i], dim_meta, private$.parent)

          abbr <- .common_axis_abbr[tolower(dim_names[i])]
          if (is.na(abbr)) abbr <- 'other'
          units <- dim_array$metadata$attributes$units %||% ''
          axes[[i]] <- switch(abbr,
                 'X' = .make_axis(dim_names[i], 'X', 'EAST', units, dim_array[]),
                 'Y' = .make_axis(dim_names[i], 'Y', 'NORTH', units, dim_array[]),
                 'Z' = .make_axis(dim_names[i], 'Z', 'UP', units, dim_array[]),
                 'T' = .make_axis(dim_names[i], 'T', 'FUTURE', units, dim_array[]),
                 .make_axis(dim_names[i], '', 'OTHER', units, dim_array[])
          )
        } else
          axes[[i]] <- .make_axis(dim_names[i], '')
      }
      axes <- setNames(axes, vapply(axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE))

      CoordinateSystem$new(name = 'local_CS', axes = axes)
    },

    # The CRS definitions in the metadata of this array, in the form of the
    # `crs` argument of set_convention(), or NULL if there are none.
    crs_from_metadata = function() {
      unproj <- function(p) {
        if (!length(p)) return(NULL)
        names(p) <- sub('^proj:', '', names(p))
        p
      }
      proj_of <- function(a) unproj(a[grepl('^proj:', names(a))])

      atts <- private$.metadata$attributes
      if (is.null(atts$cs))
        crs <- list(spatial = proj_of(atts) %||% proj_of(private$.parent$metadata$attributes))
      else {
        types <- c(planar = 'spatial', vertical = 'vertical', temporal = 'temporal')
        crs_list <- private$cs_crs(private$.metadata)
        crs <- lapply(crs_list, function(c) unproj(c$id))
        names(crs) <- types[vapply(crs_list, function(c) c$type %||% 'undefined', character(1L))]
        crs <- crs[!is.na(names(crs))]
        crs$compound <- unproj(atts$cs$id)
      }
      crs <- Filter(Negate(is.null), crs)
      if (length(crs)) crs
    },

    # Check that names passed as arguments to $subset() are valid. This means
    # that they must refer to an axis by name or abbreviation and there can be
    # no duplication. It returns an integer vector with the order in which the
    # axes are specified. This function assumes that the coordinate system is
    # set and having axes.
    check_selection_names = function(selection, axis_names) {
      if (is.null(selection) || !all(nzchar(selection)))
        stop("All selections must be named with an axis name or abbreviation", call. = FALSE)

      is_axis <- match(selection, axis_names)
      if (anyDuplicated(is_axis, incomparables = NA))
        stop("Duplicated axis names not allowed", call. = FALSE)
      if (!any(is.na(is_axis)))
        return(is_axis)

      abbr <- vapply(private$.cs$axes, function(a) a$abbreviation, character(1L))
      is_orient <- match(selection, abbr)
      if (anyDuplicated(is_orient, incomparables = NA))
        stop("Duplicated axis abbreviations not allowed", call. = FALSE)
      if (!any(is.na(is_orient)))
        return(is_orient)

      ax_na <- which(is.na(is_axis))
      is_axis[ax_na] <- is_orient[ax_na]
      if (anyNA(is_axis))
        stop("Unknown axis name or abbreviation: ", paste(selection[is.na(is_axis)], collapse = ", "), call. = FALSE)
      if (anyDuplicated(is_axis, incomparables = NA))
        stop("Duplicated axis selection by name and abbreviation not allowed", call. = FALSE)
      is_axis
    },

    # Walk `node_names` from this array like `zarr_node$walk_path()`, returning
    # the zarr_group that is found. With `create = TRUE`, missing child groups
    # are created along the way; ".." still requires an existing parent.
    # FIXME: Replace with `walk_path(node_names, create)` once available in zarr.
    walk_to_group = function(node_names, create = FALSE) {
      node <- if (create) {
        nd <- self
        for (nm in node_names) {
          nd <- if (nm == '..') {
            if (!inherits(nd$parent, 'zarr_node'))
              stop('Target Zarr node specified beyond the root node', call. = FALSE)
            nd$parent
          } else if (!inherits(nd, 'zarr_group'))
            stop('Target Zarr node specifies non-existent child node', call. = FALSE)
          else
            nd$children[[nm]] %||% nd$add_group(nm)
        }
        nd
      } else
        self$walk_path(node_names)
      if (!inherits(node, 'zarr_group'))
        stop('Target Zarr node for external arrays is not a group', call. = FALSE)
      node
    },

    # Create a new external Zarr array at `ext_name` in `grp`, or verify that an
    # existing one is compatible with `values`. `values` may be a vector (a
    # single-dimension coordinate array) or a matrix (e.g. a 2 x n bounds array),
    # in R-native shape/ordering. `dim_names` must have one entry per dimension
    # of `values`, in the same order.
    write_or_verify_external = function(grp, ext_name, values, unit, dim_names, attributes = list()) {
      shp <- dim(values) %||% length(values)

      ext_arr <- grp$children[[ext_name]]
      if (is.null(ext_arr)) {
        # No existing array -- write new array
        ext_arr <- zarr::as_zarr(x = values, name = ext_name, location = grp)
        meta <- ext_arr$metadata
        meta$dimension_names <- dim_names
        meta$attributes <- c(meta$attributes, attributes[names(attributes) != 'units'])
        meta$attributes$units <- unit
        ext_arr$metadata <- meta
        ext_arr$save()
      } else {
        # Check existing array for coincidence with `values`
        ok <- length(ext_arr$shape) == length(shp) && all(ext_arr$shape == shp) &&
          (ext_arr$attribute('units') %||% '-') == unit &&
          storage.mode(ext_vals <- ext_arr[]) == storage.mode(values) &&
          (if (is.character(values)) identical(ext_vals, values)
           else all(.near(ext_vals, values)))
        if (!ok)
          stop('Incompatible external array "', ext_name, '" already exists', call. = FALSE)
      }
      invisible(self)
    }
  ),
  public = list(
    #' @description Initialize a new GeoZarr array.
    #' @param name The name of the GeoZarr array.
    #' @param metadata List with the metadata of the array.
    #' @param parent The parent `zarr_group` instance of this new array, can be
    #'   can be a `zarr` instance if the Zarr object should have just this
    #'   array.
    #' @param coord_sys Optional, an instance of [CoordinateSystem] providing
    #'   the coordinate system of the array. If not provided, the coordinate
    #'   system is constructed from the metadata of the array persisted in the
    #'   store.
    #' @return An instance of `geozarr_array`.
    initialize = function(name, metadata, parent, coord_sys) {
      super$initialize(name, metadata, parent)
      private$.domain <- 'GeoZarr'
      private$.glyph <- '\u2316'

      # Build the coordinate system for the array - defer to the conventions and formats
      if (!missing(coord_sys) && inherits(coord_sys, 'CoordinateSystem'))
        private$.cs <- coord_sys
    },

    #' @description Perform any processing after the Zarr hierarchy is in place
    #'   and out-of-group references can be resolved.
    #' @return Self, invisibly.
    post_open = function() {
      self$build_coordsys()
    },

    #' @description Build the coordinate system of the GeoZarr array if it has
    #'   not been set yet. This should only be called when the Zarr hierarchy is
    #'   in place and out-of-group references can be resolved, particularly for
    #'   the `cs` convention.
    #' @return Self, invisibly.
    build_coordsys = function() {
      if (is.null(private$.cs)) {
        metadata <- private$.metadata
        private$.cs <- if (is.null(metadata$attributes$zarr_conventions)) {
          # No explicit convention: XArray Zarr format
          private$build_xarray(metadata)
        } else {
          # Check the convention that applies and build the coordinate system
          conv <- .conventions_supported(metadata)
          if (is.na(conv) || conv == 'spatial')
            private$build_spatial(metadata)
          else
            private$build_cs(metadata)
        }
      }
    },

    #' @description Produce a named character vector with a summary of the array
    #'   including its name, data type, shape (with axis names, including scalar
    #'   ones) and the chunking.
    #' @return Named character vector with details of the array.
    details = function() {
      nm <- private$.name
      if (!nzchar(nm))
        nm <- '(root array)'
      axes <- private$.cs$axes
      names <- names(axes)
      shp <- vapply(axes, function(ax) ax$length, integer(1L))
      shp <- paste(names, ' = ', shp, collapse = ', ')
      c(name = nm, data_type = private$.data_type$data_type, shape = shp)
    },

    #' @description This method extracts a subset of values from the GeoZarr
    #'   array, with the range along each axis to extract expressed in
    #'   coordinate values of the domain of each axis.
    #' @details The range of values along each axis to be subset is expressed in
    #'   coordinates of the domain of the axis. Any axes for which no selection
    #'   is made in the `...` argument are extracted in whole. Coordinates can
    #'   be specified in a variety of ways that are specific to the nature of
    #'   the axis. For numeric axes it should (resolve to) be a vector of real
    #'   values from which the range is computed. For time axes a vector of
    #'   character timestamps, `POSIXct` or `Date` values must be specified. As
    #'   with numeric values, only the two extreme values in the vector will be
    #'   used. For character axes the order in the axis will be used, with the
    #'   first and last value in the supplied range.
    #'
    #'   If the range of coordinate values for an axis in argument `...` extends
    #'   the valid range of the axis, the extracted data will start at the
    #'   beginning for smaller values and extend to the end for larger values.
    #'   If the values envelope the valid range the entire axis will be
    #'   extracted in the result. If the range of coordinate values for any axis
    #'   are all either smaller or larger than the valid range of the axis then
    #'   nothing is extracted and `NULL` is returned.
    #'
    #'   The extracted data has the same dimensional structure as the data in
    #'   the array, with degenerate dimensions preserved. The order of the axes
    #'   in argument `...` does not reorder the axes in the result.
    #'
    #'   Geolocation arrays of the coordinate system are not subset, so the
    #'   result has no geolocation.
    #'
    #'   Arguments following `...` must be explicitly named, like
    #'   `.rightmost.closed = TRUE`, to avoid the argument being treated as an
    #'   axis name.
    #'
    #'   As an example, to extract values of a variable for Australia for the
    #'   year 2020, where the first axis in GeoZarr array `x` is the longitude,
    #'   the second axis is the latitude, both in degrees, and the third (and
    #'   final) axis is time, the values are extracted by `x$subset(X = c(112,
    #'   154), Y = c(-9, -44), T = c("2020-01-01", "2021-01-01"))`. Note that
    #'   this works equally well for projected coordinate reference systems -
    #'   the key is that the specification in argument `...` uses the same
    #'   domain of values as the respective axes in `x` use.
    #' @param ... One or more arguments of the form `axis = range`. The "axis"
    #'   part should be the name of an axis or its abbreviation `X`, `Y`, `Z` or
    #'   `T`. The "range" part is a vector of values representing coordinates
    #'   along the axis where to extract data. Axis abbreviations and names are
    #'   case-sensitive and can be specified in any order. If values for the
    #'   range per axis fall outside of the extent of the axis, the range is
    #'   clipped to the extent of the axis.
    #' @param .name The name of the GeoZarr array to be created. If omitted, an
    #'   array will be created at the root of a new in-memory Zarr store. The
    #'   new array can then not have any associated arrays, such as for
    #'   irregular coordinate values or boundaries.
    #' @param .location Optional. If supplied, either an existing [zarr_group]
    #'   in a [zarr] object, or a character string giving the location on a
    #'   local file system where to persist the data. If the argument is a
    #'   `zarr_group`, argument `.name` must be provided. If the argument gives
    #'   the location for a new Zarr store then the location must be writable by
    #'   the calling code. As per the Zarr specification, it is recommended to
    #'   use a location that ends in ".zarr" when providing a location for a new
    #'   store. If the `.location` argument is not given, a `zarr` object is
    #'   created in memory.
    #' @return The new `geozarr_array`, with a subset of data from this GeoZarr
    #'   array, having the axes and attributes of this GeoZarr array. If one or
    #'   more of the selectors in the `...` argument fall entirely outside of
    #'   the range of the axis `NULL` is returned.
    subset = function(..., .name = NULL, .location = NULL) {
      if (is.null(private$.cs))
        stop('Cannot subset a GeoZarr array without a coordinate system set', call. = FALSE)

      axes <- private$.cs$axes
      num_axes <- length(axes)
      if (!num_axes)
        stop('Cannot subset a scalar variable', call. = FALSE)

      # Organize the selectors
      selectors <- list(...)
      if (length(selectors) == 1L && is.list(selectors[[1L]]))
        selectors <- selectors[[1L]]
      sel_names <- names(selectors)
      axis_order <- private$check_selection_names(sel_names, names(axes))

      # Subset the dimensional axes and make a new CoordinateSystem
      dims <- length(self$shape)
      out_axes <- vector('list', num_axes) # Scalar axes will be added later
      selection <- vector('list', dims)
      for (ax in seq(dims)) {
        axis <- axes[[ax]]

        # Set start and count values and create a corresponding axis
        rng <- selectors[[ axis$name ]] %||% selectors[[ axis$abbreviation ]]
        if (is.null(rng)) { # Axis not specified so take the whole axis
          idx <- c(1L, axis$length)
          out_axis <- axis$copy()
        } else { # Subset the axis
          idx <- axis$slice(rng)
          if (is.null(idx)) return(NULL)
          out_axis <- axis$subset(rng = idx)
        }
        out_axes[[ax]] <- out_axis
        selection[[ax]] <- idx
      }

      # Add any scalar axes back in
      if (dims < num_axes) {
        for (ax in (dims + 1L): num_axes) {
          out_axes[[ax]] <- axes[[ax]]$copy()
        }
      }

      names(out_axes) <- vapply(out_axes, function(ax) ax$name, character(1L), USE.NAMES = FALSE)
      cs <- CoordinateSystem$new(self$coordinate_system$name, out_axes)

      # Get the metadata of self and adjust the shape. The convention
      # attributes are replaced in .create_gza(), with the CRS of self.
      ab <- array_builder$new(self$metadata)
      ab$shape <- vapply(out_axes, function(ax) ax$length, integer(1L), USE.NAMES = FALSE)[1:dims]
      new_meta <- ab$metadata()
      new_meta$chunk_key_encoding <- self$metadata$chunk_key_encoding

      atts <- self$attributes
      atts$history <- paste0(format(Sys.time(), usetz = TRUE, digits = 0L),
                             ': Subsetting array with R package geozarr ',
                             utils::packageVersion('geozarr'), '; ', atts$history)
      new_meta$attributes <- atts

      .create_gza(meta = new_meta, cs = cs, data = self$read(selection),
                  name = .name, location = .location,
                  crs = private$crs_from_metadata(), external_group = '..')
    },

    #' @description Retrieve the geolocation arrays of this array. The arrays
    #'   are referenced from the coordinate system, for its axes with
    #'   abbreviation "X" and "Y".
    #' @details The geolocation arrays must have the names of the "X" and "Y"
    #'   axes of this array in their `dimension_names`. Any other dimension of
    #'   a geolocation array must also be a dimension of this array. The
    #'   lengths of matching dimensions must be equal; their order is not
    #'   constrained. Boundary arrays, if given, must have the dimensions of
    #'   their geolocation array plus one vertex dimension of length 4. An error
    #'   is thrown when a geolocation or boundary array cannot be found or when
    #'   its dimensions do not match.
    #' @param kind Character string, either "geodetic" (default) or "planar".
    #' @return A `list` with elements "x" and "y" holding the `zarr_array`
    #'   instances of the geolocation arrays and, if the geolocation has
    #'   boundaries, "x_bounds" and "y_bounds" holding those of the boundary
    #'   arrays; or `NULL` if this array has no geolocation of the requested
    #'   `kind`. The CRS of the geolocation arrays, if given, is available from
    #'   the `geolocation` field of the coordinate system.
    geolocation = function(kind = c('geodetic', 'planar')) {
      kind <- match.arg(kind)
      geo <- private$.cs$geolocation
      arrays <- if (!is.null(geo)) geo$as_list()[[kind]]
      if (is.null(arrays)) return(NULL)

      axes <- private$.cs$axes
      abbr <- vapply(axes, function(a) a$abbreviation, character(1L))
      xy <- list(axes[[which(abbr == 'X')]], axes[[which(abbr == 'Y')]])
      xy_names <- vapply(xy, function(a) a$name, character(1L))
      dims <- private$.metadata$dimension_names

      resolve <- function(ref, what) {
        arr <- private$resolve_external_node(node = ref$node, uri = ref$uri)
        if (!inherits(arr, 'zarr_array'))
          stop(what, ' array "', ref$node, '" not found', call. = FALSE)
        arr
      }

      out <- lapply(c(x = 'x', y = 'y'), function(k) {
        ref <- arrays[[k]]$values$ref
        arr <- resolve(ref, 'Geolocation')
        geo_dims <- unlist(arr$metadata$dimension_names)
        if (!all(xy_names %in% geo_dims))
          stop('Geolocation array "', ref$node, '" must have dimensions "',
               paste(xy_names, collapse = '" and "'), '"', call. = FALSE)
        pos <- match(geo_dims, dims)
        if (length(geo_dims) != length(arr$shape) || anyNA(pos))
          stop('Geolocation array "', ref$node, '" has dimensions that are not in the array', call. = FALSE)
        if (any(arr$shape != self$shape[pos]))
          stop('Geolocation array "', ref$node, '" does not match the lengths of the array dimensions', call. = FALSE)
        arr
      })

      if (!is.null(arrays$x$boundaries)) {
        bnds <- lapply(c(x_bounds = 'x', y_bounds = 'y'), function(k) {
          ref <- arrays[[k]]$boundaries$ref
          arr <- resolve(ref, 'Boundary')
          b_dims <- unlist(arr$metadata$dimension_names)
          v_dims <- unlist(out[[k]]$metadata$dimension_names)
          vertex <- setdiff(b_dims, v_dims)
          pos <- match(v_dims, b_dims)
          if (length(b_dims) != length(arr$shape) || anyNA(pos) || length(vertex) != 1L ||
              length(b_dims) != length(v_dims) + 1L)
            stop('Boundary array "', ref$node, '" must have the dimensions of its geolocation array and one vertex dimension', call. = FALSE)
          if (any(arr$shape[pos] != out[[k]]$shape) || arr$shape[b_dims == vertex] != 4L)
            stop('Boundary array "', ref$node, '" does not match the lengths of its geolocation array and 4 vertices', call. = FALSE)
          arr
        })
        out <- c(out, bnds)
      }
      out
    },

    #' @description Write the geolocation data of the coordinate system of
    #'   this array, set with `CoordinateSystem$set_geolocation()`, to the Zarr
    #'   arrays referenced in the geolocation object in the metadata of this
    #'   array. If an array already exists, as when geolocation arrays are
    #'   shared by multiple arrays, it is verified to be identical instead.
    #'   After writing, the data is removed from memory and the `geolocation`
    #'   field of the coordinate system references the written arrays.
    #' @details The groups that the geolocation arrays are written to must
    #'   exist, unless `create_groups = TRUE`. An error is thrown when a group
    #'   does not exist and is not created, when the metadata of this array has no
    #'   geolocation object for the data, when this array is the only array in
    #'   its Zarr store, or when an existing array differs from the data in its
    #'   dimension names, shape, units or values.
    #' @param create_groups Logical, default `FALSE`. If `TRUE`, missing groups
    #'   on the path to the geolocation arrays are created. Only groups below
    #'   the nodes that the path moves up to ("..") can be created.
    #' @return Self, invisibly.
    write_geolocation = function(create_groups = FALSE) {
      data <- private$.cs$geolocation_data
      if (!length(data)) return(invisible(self))
      if (!inherits(private$.parent, 'zarr_group'))
        stop('Single-array Zarr store cannot have geolocation arrays', call. = FALSE)

      atts <- private$.metadata$attributes
      refs <- atts$geolocation %||% do.call(c, lapply(atts$cs$crs, function(crs) crs$geolocation))
      for (kind in names(data)) {
        d <- data[[kind]]
        if (is.null(refs[[kind]]))
          stop('No "', kind, '" geolocation in the metadata of the array; use `set_convention()` first', call. = FALSE)
        write <- function(node, values, unit, dims, attributes = list()) {
          parts <- strsplit(node, '/', fixed = TRUE)[[1L]]
          name  <- parts[length(parts)]
          grp   <- private$walk_to_group(parts[-length(parts)], create_groups)
          ext   <- grp$children[[name]]
          if (!is.null(ext) && !identical(unlist(ext$metadata$dimension_names), dims))
            stop('Incompatible external array "', name, '" already exists', call. = FALSE)
          private$write_or_verify_external(grp, name, values, unit, dims, attributes)
        }
        for (xy in c('x', 'y')) {
          crd  <- refs[[kind]][[xy]]
          atts_xy <- d[[xy]]$attributes
          unit <- atts_xy$units %||% ''
          write(crd$values$ref$node, d[[xy]]$values, unit, d$dims, atts_xy)
          if (!is.null(d[[xy]]$bounds)) {
            if (is.null(crd$boundaries))
              stop('No boundaries for "', kind, '" geolocation in the metadata of the array; use `set_convention()` first', call. = FALSE)
            write(crd$boundaries$ref$node, d[[xy]]$bounds$values, unit, d$bounds_dims)
          }
        }
      }
      private$.cs$geolocation <- zarr_convention_geolocation$new(refs)
      invisible(self)
    },

    #' @description Write any external coordinates of this array to the
    #'   underlying Zarr store. The metadata of this array is scanned for `ref`
    #'   objects in the `cs` convention attributes. The values of the external
    #'   coordinates are taken from the [CoordinateSystem] instance of this
    #'   array. Any geolocation data of the coordinate system is written with
    #'   `write_geolocation()`.
    #' @param create_groups Logical, default `FALSE`. If `TRUE`, missing groups
    #'   on the path to the external arrays are created. Only groups below the
    #'   nodes that the path moves up to ("..") can be created.
    #' @return Self, invisibly.
    write_external_coordinates = function(create_groups = FALSE) {
      crs <- private$.metadata$attributes$cs$crs
      if (!is.null(crs)) {
        for (i in seq_along(crs))
          for (ax in seq_along(crs[[i]]$axes)) {
            axis_name <- names(crs[[i]]$axes)[ax]
            axis <- crs[[i]]$axes[[ax]]
            for (j in seq_along(axis$coordinates)) {
              # Coordinate values
              ext <- axis$coordinates[[j]]$values$external
              if (!is.null(ext)) {
                if (!inherits(private$.parent, 'zarr_group'))
                  stop('Single-array Zarr store cannot have external coordinate arrays', call. = FALSE)
                path_parts <- strsplit(ext$ref$node, '/', fixed = TRUE)[[1L]]
                ext_name <- path_parts[length(path_parts)]
                path_parts <- path_parts[-(length(path_parts))] # Strip the coordinate array name
                grp <- private$walk_to_group(path_parts, create_groups)
                crds <- private$.cs$axes[[axis_name]]$coordinates_list[[j]]
                values <- if (inherits(crds, 'CoordinatesTime')) crds$offsets else crds$values
                private$write_or_verify_external(grp, ext_name, values, crds$unit, ext_name)
              }

              # Boundary values
              bnd_ext <- axis$coordinates[[j]]$boundaries$external
              if (!is.null(bnd_ext)) {
                if (is.null(private$.parent))
                  stop('Single-array Zarr store cannot have external boundary arrays', call. = FALSE)
                path_parts_bnd <- strsplit(bnd_ext$ref$node, '/', fixed = TRUE)[[1L]]
                ext_bnd_name <- path_parts_bnd[length(path_parts_bnd)]
                path_parts_bnd <- path_parts_bnd[-(length(path_parts_bnd))]
                grp_bnd <- private$walk_to_group(path_parts_bnd, create_groups)
                crds <- private$.cs$axes[[axis_name]]$coordinates_list[[j]]
                private$write_or_verify_external(grp_bnd, ext_bnd_name, crds$bounds, crds$unit, c('bounds', ext_bnd_name))
              }
            }
          }
      }
      self$write_geolocation(create_groups)
      invisible(self)
    }
  ),
  active = list(
    #' @field coordinate_system (read-only) Retrieve the coordinate system of
    #'   this array.
    coordinate_system = function(value) {
      if (missing(value))
        private$.cs
    }
  )
)
