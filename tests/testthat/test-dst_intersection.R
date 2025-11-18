test_that("dst_intersection matches sf::st_intersection (fast, pair/order checks only)", {
  testthat::skip_if_not_installed("sf")
  library(sf)

  # ---------- Data ----------
  set.seed(131)
  m <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))
  p <- st_polygon(list(m))
  n <- 100
  l <- vector("list", n)
  for (i in 1:n) {
    l[[i]] <- p + 10 * runif(2)
  }
  s <- st_set_crs(st_sfc(l), 3857) # projected -> GEOS path

  # ---------- Helpers ----------
  canon_idx <- function(idx) {
    idx <- unname(idx)
    idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  }
  idx_key <- function(mat) paste(mat[, 1], mat[, 2], sep = ":")

  # ---------- sfc × sfc: compare JUST the pair set ----------
  i_sf_sfc <- sf::st_intersection(s, s)
  i_duck_sfc <- ducksf::dst_intersection(s, s, join = "intersects")

  expect_s3_class(i_sf_sfc, "sfc")
  expect_s3_class(i_duck_sfc, "sfc")

  idx_sf <- canon_idx(attr(i_sf_sfc, "idx"))
  idx_duck <- canon_idx(attr(i_duck_sfc, "idx"))
  expect_identical(idx_duck, idx_sf)

  # ---------- sf × sf: align both to the SAME canonical pair order ----------
  sf_x <- st_sf(s)
  sf_x$val <- seq_along(s)
  sf_y <- st_sf(s)
  sf_y$val <- seq_along(s)

  i_sf_sf <- sf::st_intersection(sf_x, sf_y)
  i_duck_sf <- ducksf::dst_intersection(sf_x, sf_y, join = "intersects")

  # Canonical pair order is the sfc pair order from above
  key_pairs <- idx_key(idx_sf)

  # Build row keys from the (x,y) attributes each output carries:
  # sf::st_intersection(sf,sf) names them "val" (from x) and "val.1" (from y)
  key_sf_df <- idx_key(cbind(i_sf_sf$val, i_sf_sf$val.1))
  key_duck_df <- idx_key(cbind(i_duck_sf$val, i_duck_sf$val.1))

  # Reorder both data frames to the canonical pair order
  i_sf_sf_ord <- i_sf_sf[match(key_pairs, key_sf_df), , drop = FALSE]
  i_duck_sf_ord <- i_duck_sf[match(key_pairs, key_duck_df), , drop = FALSE]

  # Same columns (allow different ordering)
  expect_setequal(names(i_duck_sf_ord), names(i_sf_sf_ord))
  # Must carry both x and y attributes with the usual duplicated-name disambiguation
  expect_true(all(c("val", "val.1") %in% names(i_duck_sf_ord)))

  # After aligning by the SAME pair order, attributes must match exactly
  expect_identical(i_duck_sf_ord$val, i_sf_sf_ord$val)
  expect_identical(i_duck_sf_ord$val.1, i_sf_sf_ord$val.1)

  # Row counts match the pair count
  expect_equal(nrow(i_sf_sf_ord), nrow(i_duck_sf_ord))
  expect_equal(nrow(i_sf_sf_ord), nrow(idx_sf))
})

# test_that("st_union/difference/sym_difference/intersection work, for all types", {
test_that("st_intersection work, for all types", {
  library(sf)
  p = st_point(0:1)
  l = st_linestring(matrix(1:10, , 2))
  pl = st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  x = list(
    pl,
    st_sfc(pl, l, pl),
    st_sf(a = 5:7, st_sfc(pl, l, pl), agr = "constant")
  )
  y = x
  # for (f in list(st_union, st_difference, st_sym_difference, st_intersection)) {
  for (f in list(dst_intersection)) {
    for (xx in x) {
      for (yy in y) {
        expect_silent(f(xx, yy))
      }
    }
  }
  # for (f in list(st_difference, st_sym_difference, st_intersection)) {
  for (f in list(dst_intersection)) {
    for (xx in x) {
      for (yy in y) {
        expect_equal(tail(class(f(xx, yy)), 1), tail(class(xx), 1))
      }
    }
  }
})
