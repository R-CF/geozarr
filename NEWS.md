# geozarr (development version)

* Geolocation v2.0.0 supported:
  * `x` and `y` hold `values` and optional `boundaries` references. `CoordinateSystem$set_geolocation()` has new arguments `x_bounds`, `y_bounds`, `bounds_dims` and `bounds_names` for the cell vertices, which `geozarr_array$write_geolocation()` writes and `geozarr_array$geolocation()` returns.
  * New class `zarr_convention_geolocation` implements the convention. `zarr_convention_cs$add_crs()` takes an instance of it for its `geolocation` argument.
  * `CoordinateSystem` has a `geolocation` field linking geolocation arrays to its X and Y axes. It is read from and written to the `cs` and `spatial` conventions. New method `geozarr_array$geolocation()` resolves the geolocation arrays.
  * New method `CoordinateSystem$set_geolocation()` sets geolocation data, such as 2-D longitude and latitude arrays, for the X and Y axes. `set_convention()` references the data in the group given by its new `geolocation_group` argument (default: `external_group`), and new method `geozarr_array$write_geolocation()` writes the arrays, or verifies arrays already written for another array, and then frees the data.
  * `geozarr_array$write_geolocation()` and `$write_external_coordinates()` have a new argument `create_groups` (default `FALSE`) to create missing groups on the path to the external arrays.
* New class `zarr_convention_multiscales` implements the "multiscales" convention (v0.1). `geozarr_group` has a new field `multiscales` giving the layout of the group. Arrays in the levels of a multiscales group that uses the "spatial" convention are opened as `geozarr_array`. The specification is ambiguous or silent on several points; this implementation reads it as follows:
  * Interpretation:
    * An `asset` may be a group or an array; which one follows from the node, not from the attributes. An array takes part in a level when the "spatial" convention is declared on the multiscales group or on the array, and both names in `spatial:dimensions` are among its `dimension_names`. Other arrays in a level group, such as coordinate arrays, are left as plain Zarr arrays.
    * `scale` is relative to the level in `derived_from`, not to the full-resolution level. `cumulative_scale()` gives the product along `derived_from` to the root. A `transform` on a level without `derived_from` is ignored there.
    * `scale` and `translation` are never used for georeferencing, because the specification does not define the dimensions they apply to, their order, or the units of `translation`. The grid of a level comes from its `spatial:transform`.
    * `spatial:` and `proj:` attributes are taken, in order, from the array, its parent group, the layout object of the level, and the multiscales group. Attributes of the multiscales group thus reach arrays that are not its direct children, and a `spatial:transform` on the multiscales group never overrides that of a level. Arrays in a level need not declare a convention themselves.
    * The multiscales layout takes precedence over XArray-style coordinate arrays in a level group.
  * Stricter than the schema:
    * The layout is rejected when an asset is listed more than once, when a `derived_from` does not name an asset in the layout, when the `derived_from` references form a cycle, or when `scale` arrays differ in length. A rejected `multiscales` attribute is ignored with a warning; the store still opens.
    * A `scale` that is not the ratio of the cell sizes in the `spatial:transform` of a level and its source gives a warning on reading and an error in `add_level()`.
    * The convention must be declared in `zarr_conventions`. Stores citing the `v1` tags are not recognised.
  * Writing with `add_level()`:
    * A level must be added after its source. `scale` is required with `derived_from`, `translation` is always zero, and no `transform` is written for a level without `derived_from`.
    * Unknown properties of layout objects are preserved; other properties of the `multiscales` object are not.
  * Not supported:
    * Levels without their own `spatial:transform`. Such a level takes the `spatial:transform` of the multiscales group when there is one, which is wrong for all but the full-resolution level; otherwise opening the store fails.
    * A multiscales group without the "spatial" convention: the layout is available but the arrays are plain Zarr arrays.
    * A non-zero `translation`.
* CRS objects defined in a group and referenced from arrays (`cs` convention) are resolved, including JSON pointers into the group metadata. References inside a shared CRS object, such as external coordinate values and geolocation arrays, are resolved relative to the group.
* `geozarr_array$hierarchy()` will show scalar axes.
* Fix potential error in selecting attributes to display.
* Unsupported conventions throw a warning instead of an error.

# geozarr 0.3.0

* Implements the new API from `zarr` 1.0.0.
* New method `geozarr_array$subset()` can subset arrays over all axes using coordinate value ranges. Better checks on selector values.
* New function `create_geozarr_array()` can create a new GeoZarr array with a coordinate system.
* Check on monotonicity of coordinate values added.
* Attributes are attached to coordinates.
* Convention matching follows the Zarr conventions spec: by UUID, then schema, then spec. A "principal" convention (providing coordinates, like "cs" and "spatial") must be declared
* Printing to the console uses the `cli` package if available. Time axis printing improved.
* `spatial` and `proj` attribute saving fixed.
* R version bumped to 4.4 to align with the `zarr` package.

# geozarr 0.2.0

* Coordinate system elements can have attributes.
* Exporting multiple objects for downstream package use.
* Boundary values are written to the array metadata.
* Better support for vertical axes.
* Better support for CRS, boundary values and external arrays.
* Bug fixes.

# geozarr 0.1.0

* Initial CRAN submission.
* This package is an extension to the `zarr` package, for reading and writing Zarr stores from memory, local file systems and the internet.
* Support for the `cs`, `spatial`, `proj`, `uom` and `ref` conventions. A coordinate system will be built from the convention attributes. "Time" coordinates will be handled using the `CFtime` package.
* The `as_geozarr()` function will convert an R object with proper `dimnames()` set to a `geozarr_array` object in a Zarr store.
