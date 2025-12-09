# Project a view's geometry into a new view (or pass-through if CRS matches)

Project a view's geometry into a new view (or pass-through if CRS
matches)

## Usage

``` r
ducksf_project_view(
  con,
  in_view,
  out_view,
  from_crs_lit,
  to_crs_lit,
  same_crs = FALSE,
  geom_col = "geom"
)
```
