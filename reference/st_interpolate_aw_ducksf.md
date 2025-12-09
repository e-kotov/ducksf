# Areal-weighted interpolation via DuckDB (no masking)

`st_interpolate_aw_ducksf()` is a user-facing, **non-masking** wrapper
that reproduces the semantics of
[`sf::st_interpolate_aw()`](https://r-spatial.github.io/sf/reference/interpolate_aw.html)
while delegating work to
[`dst_interpolate_aw()`](http://www.ekotov.pro/ducksf/reference/dst_interpolate_aw.md).
It never masks `sf`; call it explicitly.

There is also an **internal** shim, `st_interpolate_aw()` (unexported),
with the same signature. It exists so that a future, optional preference
mechanism (e.g. via the **conflicted** package) could prefer ducksf’s
implementation without changing the search path. In this package
version, no masking is performed.

## Usage

``` r
st_interpolate_aw_ducksf(x, to, extensive, ..., keep_NA = FALSE, na.rm = FALSE)

st_interpolate_aw(x, to, extensive, ..., keep_NA = FALSE, na.rm = FALSE)
```

## Arguments

- x:

  An `sf` object with source polygons and attributes to interpolate.

- to:

  An `sf` or `sfc` with target polygons (coerced via
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
  if needed).

- extensive:

  Either a single logical (sf semantics) or a character vector of column
  names to treat as spatially extensive (package extension).

- ...:

  Ignored; present for drop-in compatibility with `sf`.

- keep_NA:

  Logical; if `FALSE`, drop targets with no contributions in the
  requested variables (matches sf).

- na.rm:

  Logical; if `TRUE`, drop rows of `x` that contain any `NA` before
  interpolation (matches sf).

## Value

An `sf` object with interpolated variables merged to `to`.

## Details

Areal-weighted interpolation via DuckDB (no masking)

- **CRS** is inferred from inputs and passed to DuckDB as EPSG/WKT/JSON;
  for area work, use a projected CRS via `join_crs` in
  [`dst_interpolate_aw()`](http://www.ekotov.pro/ducksf/reference/dst_interpolate_aw.md).

- **Weights mapping (sf semantics):** if any variables are treated as
  *extensive*, the shim uses `weight = "total"`; otherwise
  `weight = "sum"`.

## See also

- [`sf::st_interpolate_aw()`](https://r-spatial.github.io/sf/reference/interpolate_aw.html)
  for the original,

- [`dst_interpolate_aw()`](http://www.ekotov.pro/ducksf/reference/dst_interpolate_aw.md)
  for the fast-path API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Explicit, no masking:
res <- ducksf::st_interpolate_aw_ducksf(x = src_sf, to = tgt_sf, extensive = TRUE)

# Original sf still available:
res_sf <- sf::st_interpolate_aw(x = src_sf, to = tgt_sf, extensive = TRUE)
} # }
```
