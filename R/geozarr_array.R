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
    # The coordinate system of this array
    .cs = NULL,

    .glyph = '\u2316',

    # Print GeoZarr details as part of printing a group.
    print_details = function() {
      cat('\nCoordinate system:\n')
      private$.cs$print_axes()
    },

    # Remove domain attributes prior to printing this geozarr_array
    display_attributes = function() {
      atts <- private$.metadata[['attributes']]
      nms <- names(atts)
      atts[!(startsWith(nms, 'spatial:') | (nms %in% c('zarr_conventions', 'cs')))]
    },

    # Create a coordinate system using the cs convention.
    build_cs = function() {
      atts            <- self$attributes
      dimension_names <- private$.metadata$dimension_names
      shape           <- private$.metadata$shape

      cs <- atts$cs
      if (is.null(cs))
        stop('Required attribute "cs" not found in array metadata.', call. = FALSE)
      if (!length(cs$crs))
        stop('Attribute "cs" must contain at least one CRS object.', call. = FALSE)

      # Collect all axis definitions from all CRS objects, keyed by dimension
      # name. Later CRS objects win on name collision (should not occur in a
      # valid store, but we need a deterministic rule).
      all_axes <- list()
      for (crs in cs$crs) {
        if (!is.null(crs$axes))
          all_axes[names(crs$axes)] <- crs$axes
      }

      # Helper: build one CoordinateSystemAxis from an axis definition.
      # dim_length is the number of elements along this dimension in the array;
      # pass 1L for scalar axes not present in dimension_names.
      # direction_override, when non-NULL, replaces whatever the metadata says.
      build_one_axis <- function(dim_name, ax_def, dim_length, direction_override = NULL) {
        if (is.null(ax_def))
          return(private$cs_ordinal_axis(dim_name, dim_length))

        abbr <- ax_def$abbreviation %||% ''
        direction <- if (is.null(direction_override)) {
          d <- toupper(ax_def$direction %||% 'OTHER')
          if (!d %in% AxisDirection) 'OTHER' else d
        } else
          direction_override

        coord_defs <- ax_def$coordinates
        if (!is.list(coord_defs) || !length(coord_defs))
          return(private$cs_ordinal_axis(dim_name, dim_length))

        coords_list <- lapply(seq_along(coord_defs), function(j) {
          private$cs_build_coordinates(coord_defs[[j]], dim_name, j, dim_length, direction)
        })
        names(coords_list) <- vapply(coords_list, function(cd) cd$name, FUN.VALUE = character(1L))

        CoordinateSystemAxis$new(name         = dim_name,
                                 abbreviation = abbr,
                                 coordinates  = coords_list)
      }

      # 1. Dimensional axes, in dimension_names order.
      axes <- lapply(seq_along(dimension_names), function(i) {
        build_one_axis(dimension_names[i], all_axes[[dimension_names[i]]], shape[i])
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
          build_one_axis(nm, all_axes[[nm]], 1L, direction_override = 'OTHER')
        })
        names(scalar_axes) <- scalar_names
        axes <- c(axes, scalar_axes)
      }

      CoordinateSystem$new(name = cs$name %||% 'cs', axes = axes)
    },

    # Build a Coordinates instance from one element of an axis's `coordinates`
    # array in the cs attribute.
    cs_build_coordinates = function(coord_def, dim_name, index, dim_length, direction) {
      # Coordinate set name: use declared name or synthesise one
      crd_name <- coord_def$name %||% paste0(dim_name, '_coordinates',
                                             if (index > 1L) index else '')

      # --- values ---
      cv <- private$cs_build_values(coord_def$values, dim_name, dim_length)

      # --- boundaries ---
      bounds <- if (!is.null(coord_def$boundaries))
        private$cs_build_bounds(coord_def$boundaries, dim_name)
      else
        NULL

      time <- coord_def$time
      if (is.null(time))
        Coordinates$new(name = crd_name, direction = direction,
                        unit = coord_def$unit %||% '', values = cv, bounds = bounds)
      else
        CoordinatesTime$new(name = crd_name, direction = direction, unit = time$unit,
                            epoch = time$epoch, calendar = time$calendar,
                            values = cv, bounds = bounds)
    },

    # Resolve a `values` object into a CoordinateValues* instance.
    cs_build_values = function(values_def, dim_name, dim_length) {
      if (is.null(values_def))
        stop('Axis "', dim_name, '": coordinate set is missing required "values" element.',
             call. = FALSE)

      if (!is.null(values_def$regular)) {
        rv <- unlist(values_def$regular)
        if (length(rv) != 2L)
          stop('Axis "', dim_name, '": "values.regular" must have exactly 2 elements.',
               call. = FALSE)
        return(CoordinateValuesNumericPacked$new(values = rv, length = dim_length))
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
          stop('Axis "', dim_name, '": "boundaries.regular" must have exactly 2 elements.', call. = FALSE)
        return(bv)
      }

      if (!is.null(bounds_def$external)) {
        raw <- private$cs_load_external(bounds_def$external, dim_name, 'boundaries')
        if (!is.matrix(raw) || nrow(raw) != 2L)
          stop('Axis "', dim_name, '": external boundaries array must be a 2-row matrix.', call. = FALSE)
        return(raw)
      }

      stop('Axis "', dim_name, '": "boundaries" must have one of "regular" or "external".', call. = FALSE)
    },

    # Load data from an `external` reference: { ref: { node, uri?, attribute? } }.
    # Only local-store refs (no `uri`) are supported; `attribute` is not yet
    # handled (it would require JSON Pointer traversal into another node's
    # attributes rather than reading array data).
    cs_load_external = function(external_def, dim_name, context) {
      ref <- external_def$ref
      if (is.null(ref) || is.null(ref$node))
        stop('Axis "', dim_name, '": ', context,
             ' external reference is missing the required "node" field.', call. = FALSE)

      if (!is.null(ref$uri))
        stop('Axis "', dim_name, '": ', context,
             ' references an external store URI "', ref$uri,
             '" which is not yet supported.', call. = FALSE)

      if (!is.null(ref$attribute))
        stop('Axis "', dim_name, '": ', context,
             ' uses an attribute JSON Pointer reference which is not yet supported.', call. = FALSE)

      # Resolve the path relative to the referring array's parent group.
      # ref$node is relative to the referring node (this array); resolve
      # relative to its parent group prefix.
      node_path     <- sub('^/', '', ref$node)  # strip any leading slash
      parent_prefix <- if (is.null(private$.parent)) '' else private$.parent$prefix
      full_prefix   <- paste0(parent_prefix, node_path, '/')

      ref_meta <- private$.store$get_metadata(full_prefix)
      if (is.null(ref_meta) || ref_meta$node_type != 'array')
        stop('Axis "', dim_name, '": ', context,
             ' external reference "', ref$node, '" does not resolve to an array.', call. = FALSE)

      ref_arr <- zarr_array$new(node_path, ref_meta, private$.parent, private$.store)
      ref_arr[]  # read all data
    },

    # Fallback: a 0-based ordinal axis for dimensions carrying no cs metadata.
    cs_ordinal_axis = function(dim_name, dim_length) {
      crd_name <- paste0(dim_name, '_coordinates')
      values   <- CoordinateValuesOrdinal$new(dim_length)
      coords   <- Coordinates$new(name = crd_name, direction = 'OTHER',
                                  unit = '', values = values)
      CoordinateSystemAxis$new(name         = dim_name,
                               abbreviation = '',
                               coordinates  = setNames(list(coords), crd_name))
    },

    # Create a coordinate system using the spatial convention
    build_spatial = function() {
      # Get spatial attributes from the parent group and merge with local attributes
      parent_atts <- if (is.null(private$.parent)) NULL else private$.parent$metadata$attributes
      atts <- self$attributes

      # Extract the parameters and check their validity
      dimension_names <- private$.metadata$dimension_names
      if (is.null(dimension_names))
        stop('Required top-level metadata item "dimension_names" not found', call. = FALSE)

      dimensions <- atts$`spatial:dimensions` %||% parent_atts$`spatial:dimensions`
      if (is.null(dimensions))
        stop('Required attribute "spatial:dimensions" not found in metadata', call. = FALSE)
      dims <- length(dimensions)
      if (dims != 2L)
        stop('Attribute "spatial:dimensions" has wrong number of items', call. = FALSE)
      dimensions <- rev(dimensions) # Get dimensions in regular order

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
      elem <- private$.metadata$shape[dim_order[1L]]
      values <- CoordinateValuesNumericPacked$new(length = elem, values = c(transform[3L], transform[1L]))
      coords <- Coordinates$new(name = paste0(dimensions[1L], '_coordinates'), direction = 'EAST',
                                unit = '', values = values,
                                bounds = if (registration == 'pixel') c(transform[1L], 0) else NULL)
      X_axis <- CoordinateSystemAxis$new(name = dimensions[1L], abbreviation = 'X',
                                         coordinates = list(X_coordinates = coords))

      elem <- private$.metadata$shape[dim_order[2L]]
      values <- CoordinateValuesNumericPacked$new(length = elem, values = c(transform[6L], transform[5L]))
      coords <- Coordinates$new(name = paste0(dimensions[2L], '_coordinates'), direction = 'NORTH',
                                unit = '', values = values,
                                bounds = if (registration == 'pixel') c(transform[5L], 0) else NULL)
      Y_axis <- CoordinateSystemAxis$new(name = dimensions[2L], abbreviation = 'Y',
                                         coordinates = list(Y_coordinates = coords))

      # Add any other axes, if present. These will all be ordinal axes as there
      # are no coordinates available in the metadata or attributes.
      others <- which(!dimension_names %in% atts$`spatial:dimensions`)
      other_axes <- if (length(others)) {
        lapply(others, function(ndx) {
          name <- dimension_names[ndx]
          values <- CoordinateValuesOrdinal$new(private$.metadata$shape[ndx])
          coords <- Coordinates$new(name = paste0(name, '_coordinates'), direction = 'OTHER', unit = '',
                                    values = values, bounds = NULL)
          axis <- CoordinateSystemAxis$new(name = name, abbreviation = '',
                                           coordinates = setNames(list(coords), paste0(name, '_coordinates')))
        })
      } else list()

      axes <- c(list(X_axis, Y_axis), other_axes)
      axes <- setNames(axes, vapply(axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE))

      CoordinateSystem$new(name = 'local_CS', axes = axes)
    },

    # Create a coordinate system from sibling "coordinate variable" arrays.
    build_xarray = function() {
      # Internal helper function
      .make_axis <- function(name, abbreviation, direction, units, values) {
        crd_name <- paste0(name, '_coordinates')
        coords <- if (missing(values))
          Coordinates$new(name = crd_name, direction = 'OTHER',
                          unit = '1', values = CoordinateValuesOrdinal$new(shape[i]), bounds = NULL)
        else
          Coordinates$new(name = crd_name, direction = direction,
                          unit = units, values = .make_coordinate_values(values), bounds = NULL)
        CoordinateSystemAxis$new(name = name, abbreviation = abbreviation,
                                 coordinates = stats::setNames(list(coords), crd_name))
      }

      meta <- self$metadata
      shape <- meta$shape
      dim_names <- meta$dimension_names %||% meta$attributes$`_ARRAY_DIMENSIONS` # Guaranteed to have one or the other
      # coordinates <- meta$attributes$coordinates # May be NULL, probably not needed because of dim_names anyway
      axes <- vector('list', length = length(shape))
      for (i in seq_along(shape)) { # Assuming dim_names and shape have the same length, as they should
        dim_prefix <- paste0(private$.parent$prefix, dim_names[i], "/")
        dim_meta   <- private$.store$get_metadata(dim_prefix)
        if (!is.null(dim_meta) && dim_meta$node_type == "array") {
          dim_array <- zarr_array$new(dim_names[i], dim_meta, private$.parent, private$.store)

          abbr <- .common_dimension_axes[tolower(dim_names[i])]
          if (is.na(abbr)) abbr <- 'unknown'
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

      CoordinateSystem$new(name = 'local_CS', axes = axes)
    }
  ),
  public = list(
    #' @description Initialize a new GeoZarr array. The array must already exist
    #'   in the store.
    #' @param name The name of the GeoZarr array.
    #' @param metadata List with the metadata of the array.
    #' @param parent The parent `zarr_group` instance of this new array, can be
    #'   missing or `NULL` if the Zarr object should have just this array.
    #' @param store The [zarr_store] instance to persist data in.
    #' @return An instance of `geozarr_array`.
    initialize = function(name, metadata, parent, store) {
      super$initialize(name, metadata, parent, store)
      private$.domain <- 'GeoZarr'

      # Build the coordinate system for the array - defer to the conventions and formats
      private$.cs <- if (is.null(metadata$attributes$zarr_conventions)) {
        # XArray Zarr format
        private$build_xarray()
      } else {
        # Check the convention that applies and build the coordinate system
        conv <- .conventions_supported(metadata)
        if (is.na(conv) || conv == 'spatial')
          private$build_spatial()
        else
          private$build_cs()
      }
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
