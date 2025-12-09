# Collect result as sf or tibble, mirroring current behavior

Collect result as sf or tibble, mirroring current behavior

## Usage

``` r
ducksf_collect_output(
  con,
  output = c("sf", "tibble"),
  target_sf,
  target_crs,
  tidQN,
  nameConflict = FALSE,
  tidOrig = NULL,
  ia_view = "interpolated_all",
  isf_view = "interpolated_sf"
)
```
