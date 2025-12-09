# Normalize a raw table into \<id, geom, ...vars\> with canonical names

Normalize a raw table into \<id, geom, ...vars\> with canonical names

## Usage

``` r
ducksf_create_norm_view(
  con,
  raw_tbl,
  out_view,
  id_col,
  out_id = c("sid", "tid"),
  geom_col_raw = "geometry",
  geom_alias = "geom",
  keep_cols = character(0)
)
```
