# geozarr (development version)

* Geolocation follows v2.0.0 of the convention: `x` and `y` hold `values` and optional `boundaries` references. `CoordinateSystem$set_geolocation()` has new arguments `x_bounds`, `y_bounds`, `bounds_dims` and `bounds_names` for the cell vertices, which `geozarr_array$write_geolocation()` writes and `geozarr_array$geolocation()` returns.
* New class `zarr_convention_geolocation` implements the "geolocation" convention (v2.0.0). `zarr_convention_cs$add_crs()` takes an instance of it for its `geolocation` argument.
* `CoordinateSystem` has a `geolocation` field linking geolocation arrays to its X and Y axes. It is read from and written to the `cs` and `spatial` conventions. New method `geozarr_array$geolocation()` resolves the geolocation arrays.
* New method `CoordinateSystem$set_geolocation()` sets geolocation data, such as 2-D longitude and latitude arrays, for the X and Y axes. `set_convention()` references the data in the group given by its new `geolocation_group` argument (default: `external_group`), and new method `geozarr_array$write_geolocation()` writes the arrays, or verifies arrays already written for another array, and then frees the data.
* `geozarr_array$write_geolocation()` and `$write_external_coordinates()` have a new argument `create_groups` (default `FALSE`) to create missing groups on the path to the external arrays.
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
