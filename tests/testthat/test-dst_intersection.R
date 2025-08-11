test_that("dst_intersection matches sf::st_intersection for binary sfc/sf", {
  testthat::skip_if_not_installed("sf")
  library(sf)

  # Fallback tolerances if helper isn't sourced
  atol <- get0(".ducksf_atol", ifnotfound = 1e-6, inherits = TRUE)
  rtol <- get0(".ducksf_rtol", ifnotfound = 1e-8, inherits = TRUE)

  set.seed(131)
  m <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))
  p <- st_polygon(list(m))
  n <- 100
  l <- vector("list", n)
  for (i in 1:n) {
    l[[i]] <- p + 10 * runif(2)
  }
  s <- st_set_crs(st_sfc(l), 3857)

  # ---------- sfc × sfc ----------
  i_sf_sfc <- sf::st_intersection(s, s)
  i_duck_sfc <- ducksf::dst_intersection(s, s, join = "intersects") # exact predicate

  expect_s3_class(i_sf_sfc, "sfc")
  expect_s3_class(i_duck_sfc, "sfc")

  # Canonicalize pair order and compare idx matrices directly
  canon <- function(idx) {
    idx <- unname(idx)
    idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  }
  idx_sf <- canon(attr(i_sf_sfc, "idx"))
  idx_duck <- canon(attr(i_duck_sfc, "idx"))
  expect_equal(idx_duck, idx_sf)

  # Compare per-feature areas (cheap & vectorized)
  order_sf <- order(attr(i_sf_sfc, "idx")[, 1], attr(i_sf_sfc, "idx")[, 2])
  order_duck <- order(
    attr(i_duck_sfc, "idx")[, 1],
    attr(i_duck_sfc, "idx")[, 2]
  )

  ge_sf <- st_geometry(i_sf_sfc)[order_sf]
  ge_duck <- st_geometry(i_duck_sfc)[order_duck]

  # Ensure CRS line up (dst_intersection should already do this, but be safe)
  if (!isTRUE(st_crs(ge_duck) == st_crs(ge_sf))) {
    ge_duck <- st_set_crs(ge_duck, st_crs(ge_sf))
  }
  expect_true(isTRUE(st_crs(ge_duck) == st_crs(ge_sf)))

  areas_sf <- as.numeric(st_area(ge_sf))
  areas_duck <- as.numeric(st_area(ge_duck))
  expect_equal(length(areas_sf), length(areas_duck))

  # Per-pair area within tolerance
  expect_true(all(
    abs(areas_sf - areas_duck) <= (atol + rtol * pmax(areas_sf, 1e-12))
  ))

  # Optional global mass check
  expect_true(
    abs(sum(areas_sf) - sum(areas_duck)) <=
      (atol + rtol * max(sum(areas_sf), 1e-12))
  )

  # ---------- sf × sf ----------
  sf_x <- st_sf(s)
  sf_y <- st_sf(s)
  sf_x$val <- seq_along(s)
  sf_y$val <- seq_along(s)

  i_sf_sf <- sf::st_intersection(sf_x, sf_y)
  i_duck_sf <- ducksf::dst_intersection(sf_x, sf_y, join = "intersects")

  # Reorder sf results to the same pair order we used above
  expect_equal(nrow(i_sf_sf), length(order_sf))
  expect_equal(nrow(i_duck_sf), length(order_duck))

  i_sf_sf_ord <- i_sf_sf[order_sf, , drop = FALSE]
  i_duck_sf_ord <- i_duck_sf[order_duck, , drop = FALSE]

  # Attribute columns present & identical
  expect_setequal(names(i_duck_sf_ord), names(i_sf_sf_ord))
  expect_true(all(c("val", "val.1") %in% names(i_duck_sf_ord)))
  expect_identical(i_duck_sf_ord$val, i_sf_sf_ord$val)
  expect_identical(i_duck_sf_ord$val.1, i_sf_sf_ord$val.1)

  # Geometry areas again (cheap check)
  ge_sf2 <- st_geometry(i_sf_sf_ord)
  ge_duck2 <- st_geometry(i_duck_sf_ord)

  if (!isTRUE(st_crs(ge_duck2) == st_crs(ge_sf2))) {
    ge_duck2 <- st_set_crs(ge_duck2, st_crs(ge_sf2))
  }
  expect_true(isTRUE(st_crs(ge_duck2) == st_crs(ge_sf2)))

  areas_sf2 <- as.numeric(st_area(ge_sf2))
  areas_duck2 <- as.numeric(st_area(ge_duck2))
  expect_true(all(
    abs(areas_sf2 - areas_duck2) <= (atol + rtol * pmax(areas_sf2, 1e-12))
  ))
})
