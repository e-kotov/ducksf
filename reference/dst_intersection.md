# Intersect geometries (DuckDB-backed, sf-compatible)

Intersect geometries (DuckDB-backed, sf-compatible)

## Usage

``` r
dst_intersection(
  x,
  y,
  duckdb_threads = getOption("ducksf.intersection.threads", NULL),
  prefilter_bbox = TRUE
)
```
