# Compile aggregation view for AW/IW (area/length/count kernels)

Compile aggregation view for AW/IW (area/length/count kernels)

## Usage

``` r
ducksf_compile_aggs(
  con,
  ex_vars = character(0),
  in_vars = character(0),
  measure = c("area", "length", "count"),
  overlap_view = "overlap",
  source_proj_view = "source_proj",
  denom_sid_view = "total_by_sid",
  denom_tid_view = "total_by_tid",
  denom_sid_alias = NULL,
  denom_tid_alias = NULL,
  out_view = "interpolated_all",
  tid_col = "tid"
)
```
