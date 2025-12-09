# Build a skinny overlap relation between projected source/target views

Build a skinny overlap relation between projected source/target views

## Usage

``` r
ducksf_build_overlap(
  con,
  src_view = "source_proj",
  trg_view = "target_proj",
  out_view = "overlap",
  join_pred = "intersects",
  measure = c("area", "length", "count"),
  keep_geom = FALSE,
  materialize = FALSE,
  dwithin_distance = NULL,
  prefilter_bbox = TRUE,
  src_id_col = "sid",
  trg_id_col = "tid",
  geom_col_name = "geom"
)
```
