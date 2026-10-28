# Shared helpers for the geozarr test suite.

# Run `expr` when the frame `env` exits (base-R equivalent of withr::defer()).
defer_in <- function(env, expr) {
  call <- substitute(expr)
  ctx <- parent.frame()
  run <- function() eval(call, ctx)
  do.call(base::on.exit, list(bquote(.(run)()), add = TRUE), envir = env)
}

# Force the plain `cat()` printing path by removing the cli theme from zarr for
# the duration of the calling test.
local_plain_theme <- function(env = parent.frame()) {
  opts <- get("Zarr.options", envir = asNamespace("zarr"))
  old <- opts$zarr_theme
  opts$zarr_theme <- NULL
  defer_in(env, opts$zarr_theme <- old)
}

# Temporarily set a geozarr option for the duration of the calling test.
local_geozarr_option <- function(key, value, env = parent.frame()) {
  old <- geozarr_options()[[key]]
  geozarr_options(key, value)
  defer_in(env, geozarr_options(key, old))
}

# Write a plain Zarr array into `grp` with explicit dimension names and extra
# attributes, as an XArray-style producer would. Returns the new array.
write_plain_array <- function(grp, x, name, dims, atts = list()) {
  zarr::as_zarr(x = x, name = name, location = grp) # returns `grp`, not the array
  a <- grp$children[[name]]
  m <- a$metadata
  m$dimension_names <- dims
  m$attributes <- c(m$attributes, atts)
  a$metadata <- m
  a$save()
  a
}

axis_names <- function(gza)
  vapply(gza$coordinate_system$axes, function(ax) ax$name, character(1L), USE.NAMES = FALSE)
