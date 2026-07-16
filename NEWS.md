# geozarr 0.1.0

* Initial CRAN submission.
* This package is an extension to the `zarr` package, for reading and writing Zarr stores from memory, local file systems and the internet.
* Support for the `cs`, `spatial`, `proj`, `uom` and `ref` conventions. A coordinate system will be built from the convention attributes. "Time" coordinates will be handled using the `CFtime` package.
* The `as_geozarr()` function will convert an R object with proper `dimnames()` set to a `geozarr_array` object in a Zarr store.
