
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geozarr

<!-- badges: start -->

<!-- badges: end -->

The `geozarr` package implements a set of conventions for geospatial
data on top of the Zarr specification. It builds on the `zarr` package,
a native R implementation of the Zarr specification that can read and
write Zarr v.3 stores in memory, on the local file system and over HTTP.

The following conventions are supported by `geozarr`:

- [cs](https://github.com/R-CF/zarr_conventions_cs): Comprehensive
  support for any kind of axis, with CF-compatible constructs.
- [spatial](https://github.com/zarr-conventions/spatial): Compact
  coordinate system for X-Y (image, GIS) data.
- [proj](https://github.com/zarr-conventions/proj): Reference frame to
  register a coordinate system to Earth.
- [uom](https://github.com/clbarnes/zarr-convention-uom):
  Unit-of-measure information for data in a Zarr array.
- [ref](https://github.com/R-CF/zarr_convention_ref): A standard way to
  refer to Zarr objects or attributes elsewhere in the store or in other
  stores.

# Working with `geozarr`

The `geozarr` package is closely integrated with the `zarr` package, to
the extent that the only user-facing function in this package is
`as_geozarr()`, to convert an R object (vector, matrix, array) into a
Zarr array or store with GeoZarr metadata. Manipulating the Zarr object
is done with the same tools as a regular Zarr object.

# Development

GeoZarr is currently under active development and this package is
similarly in flux. The conventions implemented in this package will
remain available unless the convention is deprecated due to any reason
that would recommend against continuing to use the convention.

This package should currently not be used for production environments.
Things may fail and you are advised to ensure that you have backups of
all data that you put in a Zarr store with this package.

Like GeoZarr itself, this package is modular and allows for additional
conventions to be added to this basic implementation. If you have
specific needs, open an [issue on
Github](https://github.com/R-CF/geozarr/issues) or, better yet, fork the
code and submit code suggestions via a pull request. Specific guidance
for developers is being drafted.

Installation from CRAN of the latest release:

    install.packages("geozarr")

You can install the development version of `geozarr` from
[GitHub](https://github.com/R-CF/geozarr) with:

    # install.packages("devtools")
    devtools::install_github("R-CF/geozarr")
