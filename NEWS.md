# geozarr (development version)

* Implements the new API from `zarr` 1.0.0.
* New method `geozarr_array$subset()` can subset arrays over all axes using coordinate value ranges.
* New function `create_geozarr_array()` can create a new GeoZarr array with a coordinate system.
* Check on monotonicity of coordinate values added.
* Attributes are attached to coordinates.
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
