# Areal-weighted interpolation of polygon data (DuckDB-backed)

A fast, DuckDB Spatial-backed reimplementation of areal-weighted
interpolation found in
[`areal::aw_interpolate()`](https://slu-openGIS.github.io/areal/reference/aw_interpolate.html)
and
[`sf::st_interpolate_aw()`](https://r-spatial.github.io/sf/reference/interpolate_aw.html).
It performs all calculations using a temporary in-memory DuckDB
database.

## Usage

``` r
dst_interpolate_aw(
  target_sf, tid, source_sf, sid, weight = "sum", output = "sf",
  extensive, intensive, source_crs = NULL, target_crs = NULL, join_crs = NULL,
  duckdb_threads = NULL, na.rm = FALSE, keep_NA = TRUE
)
```

## Arguments

- target_sf:

  A `sf` object with polygons to which values are interpolated (the
  target geometry).

- tid:

  A column in `target_sf` that uniquely identifies target features.

- source_sf:

  A `sf` object with polygons providing the values to interpolate (the
  source geometry).

- sid:

  A column in `source_sf` that uniquely identifies source features.

- weight:

  For spatially extensive variables, either `"sum"` (default) or
  `"total"`. For spatially intensive variables, use `"sum"`.

- output:

  One of `"sf"` (default) or `"tibble"` for the return type.

- extensive:

  Character vector of column names in `source_sf` treated as spatially
  extensive (e.g., counts). Optional if `intensive` is supplied.

- intensive:

  Character vector of column names in `source_sf` treated as spatially
  intensive (e.g., rates/densities). Optional if `extensive` is
  supplied.

- source_crs, target_crs:

  Optional CRS specifications for the source/target inputs. If omitted,
  they are inferred from `sf::st_crs(source_sf)` and
  `sf::st_crs(target_sf)` respectively. Accepts EPSG code (numeric) or
  WKT/PROJJSON (character).

- join_crs:

  Optional CRS used for the spatial join and area calculations. If
  omitted, defaults to `target_crs`. **Use a projected CRS** for
  area-based work.

- duckdb_threads:

  Optional integer: number of DuckDB threads. If `NULL`, the DuckDB
  default is used.

- na.rm:

  Logical; if `TRUE`, drops rows from `source_sf` with any `NA` in the
  variables being interpolated. Defaults to `FALSE`.

- keep_NA:

  Logical; if `FALSE`, drops targets where all requested interpolated
  variables are `NA`. Defaults to `TRUE`.

## Value

An `sf` object (default) with interpolated variables merged to
`target_sf`, or a `tibble` when `output = "tibble"`.

## Details

Areal-weighted interpolation estimates values for overlapping but
non-congruent polygons and assumes the attribute is evenly distributed
across each source polygon. Results are most reliable when a projected
CRS is used for area calculations.

**CRS handling.** DuckDB does not persist CRS metadata. CRS values used
here are **inferred from the provided `sf` objects** via
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
and then passed to DuckDB Spatial as EPSG/WKT/PROJJSON literals for
transformations. If `source_crs`/`target_crs` are not supplied, they
default to the CRS of the corresponding `sf` objects. `join_crs`
defaults to `target_crs`. For area-based workflows, supply a
**projected** `join_crs`.

**Weights.** For spatially extensive variables, `weight = "sum"` uses
the *intersected* area per source feature as the denominator;
`weight = "total"` uses the full source polygon area. For spatially
intensive variables, denominators are computed per target using the sum
of intersected areas and `weight = "sum"`.

## References

Prener, C. and Revord, C. (2019). *areal: An R package for areal
weighted interpolation*. *Journal of Open Source Software*, 4(37), 1221.
Available at: <https://doi.org/10.21105/joss.01221>

## See also

[`areal::aw_interpolate()`](https://slu-openGIS.github.io/areal/reference/aw_interpolate.html)
— reference implementation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sketch (using your own sf objects):
res <- dst_interpolate_aw(
  target_sf = target,
  tid       = target_id,
  source_sf = source,
  sid       = source_id,
  weight    = "sum",
  extensive = c("pop_total", "jobs"),
  intensive = "density",
  join_crs  = 3857
)
} # }
```
