test_that("masked ducksf shim matches sf::st_interpolate_aw (sf semantics)", {
  testthat::skip_if_not_installed("sf")
  library(sf)

  # Data
  race <- areal::ar_stl_race
  wards <- areal::ar_stl_wards

  # sf baseline (non-mass-preserving when coverage incomplete)
  res_sf <- sf::st_interpolate_aw(
    x = race["TOTAL_E"],
    to = wards,
    extensive = TRUE,
    keep_NA = FALSE, # sf default
    na.rm = FALSE
  )

  # Enable overrides and run masked call
  on.exit(ducksf::sf_use_ducksf(FALSE, silent = TRUE), add = TRUE)
  options(ducksf.aw.join_predicate = "intersects")
  enabled <- ducksf::sf_use_ducksf(TRUE, silent = TRUE)

  res_masked <- st_interpolate_aw(
    x = race["TOTAL_E"],
    to = wards,
    extensive = TRUE,
    keep_NA = FALSE, # match sf
    na.rm = FALSE
  )

  # Map both results back to WARD using target indices in row names
  idx_sf <- as.integer(row.names(res_sf))
  idx_masked <- as.integer(row.names(res_masked))

  tbl_sf <- dplyr::tibble(
    WARD = wards$WARD[idx_sf],
    TOTAL_E_sf = as.numeric(sf::st_drop_geometry(res_sf)$TOTAL_E)
  )
  tbl_masked <- dplyr::tibble(
    WARD = wards$WARD[idx_masked],
    TOTAL_E_masked = as.numeric(sf::st_drop_geometry(res_masked)$TOTAL_E)
  )

  cmp <- merge(tbl_sf, tbl_masked, by = "WARD", all = TRUE)

  # 1) Per-target agreement
  mad <- max_abs_diff(cmp$TOTAL_E_sf, cmp$TOTAL_E_masked)
  expect_lt(
    mad,
    .ducksf_atol + .ducksf_rtol * max(abs(cmp$TOTAL_E_sf), na.rm = TRUE)
  )

  # 2) Totals equal between sf and masked shim (sf semantics replicated)
  total_sf <- sum_numeric(tbl_sf$TOTAL_E_sf)
  total_masked <- sum_numeric(tbl_masked$TOTAL_E_masked)
  expect_true(almost_equal(total_sf, total_masked))
})
