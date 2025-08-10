# ducksf. Some Alternatives to sf Functions And This is a Working Title


<!-- README.md is generated from README.qmd. Please edit that file -->

# ducksf

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ducksf.png)](https://CRAN.R-project.org/package=ducksf)
<!-- badges: end -->

This package provides some alternatives to sf functions, which are
implemented using duckdb and geoarrow. It is intended to be used with
duckdb and duckspatial. I have no idea where this project will go, use
at your own risk.

`ducksf` provides **fast, DuckDB Spatial–backed spatial data
operations** for R,  
with a focus on **areal-weighted interpolation** as a drop-in
alternative to  
`sf::st_interpolate_aw()` and `areal::aw_interpolate()`.

## Installation

You can install the development version of ducksf from
[GitHub](https://github.com/e-kotov/ducksf) with:

``` r
# install.packages("pak")
pak::pak("e-kotov/ducksf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## Quick example (using the default `sf` dataset)
library(sf)
library(ducksf)

# data & grid (from sf help)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
g <- st_make_grid(nc, n = c(10, 5)) # 'to' can be sfc for sf, but ducksf core needs an sf with an ID

# Prepare IDs for ducksf core
nc$src_id <- seq_len(nrow(nc))
g_sf <- st_as_sf(g)
g_sf$tid <- seq_len(nrow(g_sf))

## 1) ducksf core (dst_interpolate_aw) --------------------------------------

# (a) Treat BIR74 as spatially intensive (not mass-preserving)
a1_core <- dst_interpolate_aw(
  target_sf = g_sf,
  tid = "tid",
  source_sf = nc,
  sid = "src_id",
  weight = "sum", # intensive semantics
  intensive = "BIR74",
  output = "sf"
)
sum(st_drop_geometry(a1_core)$BIR74, na.rm = TRUE) / sum(nc$BIR74, na.rm = TRUE)

# (b) Treat BIR74 as spatially extensive (mass-preserving)
a2_core <- dst_interpolate_aw(
  target_sf = g_sf,
  tid = "tid",
  source_sf = nc,
  sid = "src_id",
  weight = "total", # extensive semantics (pycnophylactic)
  extensive = "BIR74",
  output = "sf"
)
sum(st_drop_geometry(a2_core)$BIR74, na.rm = TRUE) / sum(nc$BIR74, na.rm = TRUE)

# Quick plot (intensive vs extensive)
a_show <- a1_core[, "BIR74"]
names(a_show)[1] <- "intensive"
a_show$extensive <- st_drop_geometry(a2_core)$BIR74
plot(a_show[c("intensive", "extensive")], key.pos = 4)

## 2) Masked drop-in (sf semantics) -----------------------------------------

# Enable masking so st_interpolate_aw() dispatches to ducksf
sf_use_ducksf(TRUE) # or: sf_use_ducksf(TRUE, allow = "st_interpolate_aw")
sf_use_ducksf(FALSE) # or: sf_use_ducksf(TRUE, allow = "st_interpolate_aw")

a1 <- st_interpolate_aw(nc["BIR74"], g, extensive = FALSE)
sum(a1$BIR74) / sum(nc$BIR74) # not close to 1 (intensive)

a2 <- st_interpolate_aw(nc["BIR74"], g, extensive = TRUE)
sum(a2$BIR74) / sum(nc$BIR74) # ≈ 1 (mass-preserving)

# Compare maps exactly like sf's example
a1$intensive <- a1$BIR74
a1$extensive <- a2$BIR74
plot(a1[c("intensive", "extensive")], key.pos = 4)

# Disable masking when done
sf_use_ducksf(FALSE)
```

# To test speed on relatively large data

``` r
library(duckdb)
library(sf)
library(areal)
library(geoarrow)
library(terra)
library(geodata)
library(areal)
library(tidyverse)
library(ducksf)


# download may take > 5s
dir.create("private/data", recursive = TRUE)
pop <- geodata::population(year = 2020, res = 2.5, path = "private/data") # GPWv4 density
nuts2 <- giscoR::gisco_get_nuts(year = "2021", nuts_level = 2, epsg = "4326")

fr2 <- subset(nuts2, CNTR_CODE == "FR")

fr2_mainland <- subset(fr2, !grepl("^FRY|^FRM", NUTS_ID))
# plot(st_geometry(fr2_mainland), main = "France NUTS-2 regions (2021)")

# crop pop raster to mainland France
pop_fr <- terra::crop(
  pop,
  st_bbox(fr2_mainland),
  snap = "out"
)

# mask
pop_fr <- terra::mask(pop_fr, fr2_mainland)
# plot(pop_fr, main = "GPWv4 Population Density in Mainland France (2020)")

# project the raster to EPSG:3035 (ETRS89 / LAEA Europe)
pop_fr <- terra::project(
  pop_fr,
  "EPSG:3035",
  method = "bilinear"
)


# vectorize the raster
pop_fr_vec <- as.polygons(pop_fr, aggregate = FALSE) |>
  st_as_sf() |>
  mutate(cell_id = as.character(row_number()))

# generate a hexagonal grid over mainland France
bbox_fr <- st_bbox(st_transform(fr2_mainland, 3035))
cellsize_hex <- 2500 # 2.5 km hexagons, also try pushing it down to 1000 m
hex <- st_make_grid(bbox_fr, cellsize = cellsize_hex, square = FALSE) |>
  st_as_sf() |>
  st_set_geometry("geometry") |>
  mutate(hex_id = as.character(row_number()))
nrow(hex)
format(object.size(hex), "Mb")


aw_test <- ar_validate(
  source = pop_fr_vec,
  target = hex,
  varList = "population_density",
  method = "aw",
  verbose = TRUE
)
aw_test


Sys.time()
tictoc::tic()
fr_areal <- aw_interpolate(
  .data = hex,
  tid = hex_id,
  source = pop_fr_vec,
  sid = cell_id,
  weight = "sum",
  output = "tibble",
  extensive = "population_density"
)
tictoc::toc()
Sys.time()


Sys.time()
tictoc::tic()
hex_dst <- dst_interpolate_aw(
  target_sf = hex,
  tid = "hex_id",
  source_sf = pop_fr_vec,
  sid = "cell_id",
  weight = "total",
  output = "tibble",
  extensive = "population_density",
  target_crs = st_crs(hex),
  source_crs = st_crs(pop_fr_vec),
  join_crs = st_crs(hex)
)
tictoc::toc()
Sys.time()


Sys.time()
tictoc::tic()
bm <- bench::mark(
  sf_areal_aw = aw_interpolate(
    .data = hex,
    tid = hex_id,
    source = pop_fr_vec,
    sid = cell_id,
    weight = "sum",
    output = "tibble",
    extensive = "population_density"
  ),
  ducksf_aw_v3 = dst_interpolate_aw(
    target_sf = hex,
    tid = "hex_id",
    source_sf = pop_fr_vec,
    sid = "cell_id",
    weight = "total",
    output = "tibble",
    extensive = "population_density",
    target_crs = st_crs(hex),
    source_crs = st_crs(pop_fr_vec),
    join_crs = st_crs(hex),
    duckdb_threads = NULL
  ),
  check = FALSE,
  iterations = 5
)
tictoc::toc()
Sys.time()
bm
```
