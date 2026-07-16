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
        if ((len <- length(parts)) < 2L)
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

      if (missing(attribute))
        referred_node
      else {
        # Get the requested attribute from referred_node
        stop('Not yet supported', call. = FALSE)
      }
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

      # Collect all axis definitions from all CRS objects, keyed by dimension
      # name. Later CRS objects win on name collision (should not occur in a
      # valid store, but we need a deterministic rule).
      all_axes <- list()
      for (crs in cs$crs) {
        if (!is.null(crs$axes))
          all_axes[names(crs$axes)] <- crs$axes
      }

      # Internal function: Build one CoordinateSystemAxis from an axis definition.
      # dim_length is the number of elements along this dimension in the array;
      # pass 1L for scalar axes not present in dimension_names.
      build_one_axis <- function(dim_name, ax_def, dim_length) {
        if (is.null(ax_def))
          return(private$cs_ordinal_axis(dim_name, dim_length))

        abbr <- ax_def$abbreviation %||% ''
        direction <- toupper(ax_def$direction %||% 'OTHER')
        if (!direction %in% AxisDirection) direction <- 'OTHER'

        coord_defs <- ax_def$coordinates
        if (!is.list(coord_defs) || !length(coord_defs))
          return(private$cs_ordinal_axis(dim_name, dim_length))

        coords_list <- lapply(seq_along(coord_defs), function(j) {
          private$cs_build_coordinates(coord_defs[[j]], dim_name, j, dim_length, direction)
        })
        names(coords_list) <- vapply(coords_list, function(cd) cd$name, FUN.VALUE = character(1L))

        CoordinateSystemAxis$new(name = dim_name, abbreviation = abbr, coordinates = coords_list)
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
          ax <- build_one_axis(nm, all_axes[[nm]], 1L)
          ax$coordinates$direction <- 'OTHER'
          ax
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
                                unit = coord_def$unit %||% '', values = cv$values, length = cv$length, bounds = bounds)
        else
          Coordinates$new(name = crd_name, direction = direction,
                          unit = coord_def$unit %||% '', values = cv$values, bounds = bounds)
      } else {
        values <- cv$values
        if (!is.null(cv$length))
          values <- seq(from = values[1L], by = values[2L], length.out = cv$length)
        CoordinatesTime$new(name = crd_name, direction = direction, unit = time$unit,
                            epoch = time$epoch, calendar = time$calendar,
                            values = values, bounds = bounds)
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
      ref_arr$read()
    },

    # Fallback: a 0-based ordinal axis for dimensions carrying no cs metadata.
    cs_ordinal_axis = function(dim_name, dim_length) {
      crd_name <- paste0(dim_name, '_coordinates')
      values   <- CoordinateValuesOrdinal$new(dim_length)
      coords   <- Coordinates$new(name = crd_name, direction = 'OTHER', unit = '', values = values)
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
      coords <- CoordinatesPacked$new(name = paste0(dimensions[2L], '_coordinates'), direction = 'EAST',
                                      unit = '', values = c(transform[3L], transform[1L]),
                                      length = meta$shape[dim_order[2L]],
                                      bounds = if (registration == 'pixel') c(transform[1L], 0) else NULL)
      X_axis <- CoordinateSystemAxis$new(name = dimensions[2L], abbreviation = 'X',
                                         coordinates = list(X_coordinates = coords))

      coords <- CoordinatesPacked$new(name = paste0(dimensions[1L], '_coordinates'), direction = 'NORTH',
                                      unit = '', values = c(transform[6L], transform[5L]),
                                      length = meta$shape[dim_order[1L]],
                                      bounds = if (registration == 'pixel') c(transform[5L], 0) else NULL)
      Y_axis <- CoordinateSystemAxis$new(name = dimensions[1L], abbreviation = 'Y',
                                         coordinates = list(Y_coordinates = coords))

      # Add any other axes, if present. These will all be ordinal axes as there
      # are no coordinates available in the metadata or attributes.
      others <- which(!dimension_names %in% atts$`spatial:dimensions`)
      other_axes <- if (length(others)) {
        lapply(others, function(ndx) {
          name <- dimension_names[ndx]
          coords <- CoordinatesOrdinal$new(name = paste0(name, '_coordinates'), direction = 'OTHER',
                                           length = private$.metadata$shape[ndx])
          axis <- CoordinateSystemAxis$new(name = name, abbreviation = '',
                                           coordinates = setNames(list(coords), paste0(name, '_coordinates')))
        })
      } else list()

      axes <- c(list(X_axis, Y_axis), other_axes)
      axes <- setNames(axes, vapply(axes, function(ax) ax$name, FUN.VALUE = character(1L), USE.NAMES = FALSE))

      CoordinateSystem$new(name = 'local_CS', axes = axes)
    },

    # Create a coordinate system from sibling "coordinate variable" arrays.
    build_xarray = function(meta) {
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

      shape <- meta$shape
      dim_names <- meta$dimension_names %||% meta$attributes$`_ARRAY_DIMENSIONS` # Guaranteed to have one or the other
      # coordinates <- meta$attributes$coordinates # May be NULL, probably not needed because of dim_names anyway
      axes <- vector('list', length = length(shape))
      for (i in seq_along(shape)) { # Assuming dim_names and shape have the same length, as they should
        dim_prefix <- paste0(private$.parent$prefix, dim_names[i], "/")
        dim_meta   <- private$.store$get_metadata(dim_prefix)
        if (!is.null(dim_meta) && dim_meta$node_type == "array") {
          dim_array <- zarr_array$new(dim_names[i], dim_meta, private$.parent, private$.store)

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

      CoordinateSystem$new(name = 'local_CS', axes = axes)
    },

    # Check that names passed as arguments to $subset() are valid. This means
    # that they must refer to an axis by name or abbreviation and there can be
    # no duplication. It returns an integer vector with the order in which the
    # axes are specified. This function assumes that the coordinate system is
    # set and having axes.
    check_selection_names = function(selection, axis_names) {
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
      is_axis
    }
  ),
  public = list(
    #' @description Initialize a new GeoZarr array.
    #' @param name The name of the GeoZarr array.
    #' @param metadata List with the metadata of the array.
    #' @param parent The parent `zarr_group` instance of this new array, can be
    #'   missing or `NULL` if the Zarr object should have just this array.
    #' @param store The [zarr_store] instance to persist data in.
    #' @param coord_sys Optional, an instance of [CoordinateSystem] providing
    #'   the coordinate system of the array. If not provided, the coordinate
    #'   system is constructed from the metadata of the array persisted in the
    #'   store.
    #' @return An instance of `geozarr_array`.
    initialize = function(name, metadata, parent, store, coord_sys) {
      super$initialize(name, metadata, parent, store)
      private$.domain <- 'GeoZarr'

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
    #' @param .rightmost.closed Optional. Single logical value to indicate if
    #'   the upper boundary of range in each axis should be included.
    #' @param .name The name of the GeoZarr array to be created. If omitted, an
    #'   array will be created at the root of a new in-memory Zarr store.
    #' @param .location Optional. If supplied, either an existing [zarr_group]
    #'   in a [zarr] object, or a character string giving the location on a
    #'   local file system where to persist the data. If the argument is a
    #'   `zarr_group`, argument `.name` must be provided. If the argument gives
    #'   the location for a new Zarr store then the location must be writable by
    #'   the calling code. As per the Zarr specification, it is recommended to
    #'   use a location that ends in ".zarr" when providing a location for a new
    #'   store. If argument `.name` is given then the `geozarr_array` will be
    #'   created in the root of the `zarr` store with that name. If the `.name`
    #'   argument is not given, a single-array Zarr store will be created. If
    #'   the `location` argument is not given, a `zarr` object is created in
    #'   memory.
    #' @return If the `.location` argument is a `zarr_group`, the new Zarr
    #'   `geozarr_array` is returned, with a subset of data from this GeoZarr
    #'   array, having the axes and attributes of this GeoZarr array. Otherwise,
    #'   the `zarr` object that is newly created and which contains the
    #'   `geozarr_array` instance, or an error if the `zarr` object could not be
    #'   created. If one or more of the selectors in the `...` argument fall
    #'   entirely outside of the range of the axis `NULL` is returned.
    subset = function(..., .name = NULL, .location = NULL, .rightmost.closed = FALSE) {
      if (is.null(private$.cs))
        stop('Cannot subset a GeoZarr array without a coordinate system set', call. = FALSE)
      if (!missing(.name) && !zarr::is_valid_node_name(.name))
        stop('Invalid name for a Zarr array: ', .name, call. = FALSE)

      axes <- private$.cs$axes
      num_axes <- length(axes)
      if (!num_axes)
        stop('Cannot subset a scalar variable', call. = FALSE)

      # Organize the selectors
      selectors <- list(...)
      if (is.list(selectors[[1L]]))
        selectors <- selectors[[1L]]
      sel_names <- names(selectors)
      axis_order <- private$check_selection_names(sel_names, names(axes))

      # Subset the axes and make a new CoordinateSystem
      out_axes <- vector('list', num_axes)
      selection <- vector('list', num_axes)
      for (ax in seq(num_axes)) {
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
      names(out_axes) <- vapply(out_axes, function(ax) ax$name, character(1L), USE.NAMES = FALSE)

      cs <- CoordinateSystem$new(self$coordinate_system$name, out_axes)

      # Get the metadata of self and adjust the shape
      ab <- array_builder$new(self$metadata)
      ab$shape <- vapply(out_axes, function(ax) ax$length, integer(1L), USE.NAMES = FALSE)
      new_meta <- .geozarr_set_convention(ab$metadata(), cs, '..')
      new_meta$chunk_key_encoding <- self$metadata$chunk_key_encoding

      # Create the new GeoZarr array
      if (inherits(.location, 'zarr_group')) {
        # New gza at the location in the implicit zarr object: return the gza
        gza <- geozarr_array$new(name = .name, metadata = new_meta, parent = .location, store = .location$store, coord_sys = cs)
        gza$write(self$read(selection))
        .location$set_node(gza)
      } else {
        # Create the store and add the array to make the store valid
        store <- if (missing(.location) || is.null(.location) || !nzchar(.location))
          zarr::zarr_memorystore$new()
        else
          zarr::zarr_localstore$new(root = .location)

        if (missing(.name) || is.null(.name) || !nzchar(.name)) {
          .name <- ''
          store$create_array(name = '', metadata = new_meta)
        } else {
          store$create_group(name = '')
          store$create_array(parent = '/', name = .name, metadata = new_meta)
        }

        # Create the Zarr object and get a handle on the newly created array
        z <- zarr$new(store)
        gza <- z[[paste0('/', .name)]]
        gza$write(self$read(selection))
        z
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

# ================= Helper functions ===========================================

# This function writes the external coordinate arrays to the Zarr store. The
# meta argument is scanned for ref objects in the `cs` convention attributes.
# The values are taken from the coord_sys argument, a CoordinateSystem instance.
# The relative path is computed against the path of argument arr, a zarr_array
# instance.
.write_external_coordinates <- function(meta, coord_sys, arr) {
  crs <- meta$attributes$cs$crs
  if (!is.null(crs)) {
    for (c in seq_along(crs))
      for (ax in seq_along(crs[[c]]$axes)) {
        axis_name <- names(crs[[c]]$axes)[ax]
        axis <- crs[[c]]$axes[[ax]]
        ext <- axis$coordinates[[1L]]$values$external
        if (!is.null(ext)) {
          if (is.null(arr$parent))
            stop('Single-array Zarr store cannot have external coordinate arrays', call. = FALSE)
          path_parts <- strsplit(ext$ref$node, '/', fixed = TRUE)[[1L]]
          path_parts <- path_parts[-(length(path_parts))] # Strip the array name
          grp <- arr$walk_path(path_parts)
          sibling_name <- paste0(axis_name, '_coord')
          crds <- coord_sys$axes[[axis_name]]$coordinates
          values <- if (inherits(crds, 'CoordinatesTime')) crds$offsets else crds$values
          sibling <- zarr::as_zarr(x = values, name = sibling_name, location = grp)
          sibling_metadata <- sibling$metadata
          sibling_metadata$dimension_names <- sibling_name
          sibling$metadata <- sibling_metadata
          sibling$save()
          grp$set_node(sibling)
        }
      }
  }
}
