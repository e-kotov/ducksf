# Build denominator table/view for areal/length/count kernels

Build denominator table/view for areal/length/count kernels

## Usage

``` r
ducksf_build_denom(
  con,
  by = c("sid", "tid"),
  mode = NULL,
  measure = c("area", "length", "count"),
  overlap_view = "overlap",
  src_proj_view = "source_proj",
  out_view = NULL,
  id_col = NULL,
  alias = NULL
)
```
