# Build normalized + (optionally) projected source/target views

Build normalized + (optionally) projected source/target views

## Usage

``` r
ducksf_build_proj_views(
  con,
  sidQN,
  tidQN,
  src_lit,
  trg_lit,
  join_lit,
  same_src_join,
  same_trg_join,
  vars = character(0),
  source_tbl_raw = "source_tbl_raw",
  target_tbl_raw = "target_tbl_raw",
  src_geom_name = "geometry",
  trg_geom_name = "geometry"
)
```
