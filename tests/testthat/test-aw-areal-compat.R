test_that("ducksf core matches areal for extensive vars (mass-preserving)", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("areal")

  library(sf)
  library(areal)

  race <- areal::ar_stl_race
  wards <- areal::ar_stl_wards

  # areal: tibble output with TOTAL_E interpolated to wards
  res_areal <- areal::aw_interpolate(
    wards,
    tid = WARD,
    source = race,
    sid = GEOID,
    weight = "sum",
    output = "tibble",
    extensive = "TOTAL_E"
  )

  # ducksf core: same semantics (projected join; intersects)
  res_ducksf <- ducksf::dst_interpolate_aw(
    target_sf = wards,
    tid = "WARD",
    source_sf = race,
    sid = "GEOID",
    weight = "sum",
    output = "tibble",
    extensive = "TOTAL_E",
    na.rm = FALSE,
    keep_NA = TRUE
  )

  # Join by WARD; compare value columns
  cmp <- merge(
    res_areal[, c("WARD", "TOTAL_E")],
    res_ducksf[, c("WARD", "TOTAL_E")],
    by = "WARD",
    suffixes = c("_areal", "_ducksf"),
    all = TRUE
  )

  # 1) Per-target agreement within tolerance
  mad <- max_abs_diff(cmp$TOTAL_E_areal, cmp$TOTAL_E_ducksf)
  expect_lt(
    mad,
    .ducksf_atol + .ducksf_rtol * max(abs(cmp$TOTAL_E_areal), na.rm = TRUE)
  )

  # 2) Mass preservation (extensive) both should equal source sum
  src_total <- sum_numeric(race$TOTAL_E)
  areal_total <- sum_numeric(cmp$TOTAL_E_areal)
  ducksf_total <- sum_numeric(cmp$TOTAL_E_ducksf)

  expect_true(almost_equal(areal_total, src_total))
  expect_true(almost_equal(ducksf_total, src_total))
})


test_that("ddbs_interpolate_aw handles different CRS", {
  # Create a source layer with a different CRS (WGS 84)
  race_wgs84 <- sf::st_transform(race, 4326)

  # Result with native CRS
  res_native_crs <- dst_interpolate_aw(
    target_sf = wards,
    tid = WARD,
    source_sf = race,
    sid = GEOID,
    extensive = "TOTAL_E"
  )

  # Result with different source CRS should be identical after internal reprojection
  res_diff_crs <- dst_interpolate_aw(
    target_sf = wards,
    tid = WARD,
    source_sf = race_wgs84,
    sid = GEOID,
    extensive = "TOTAL_E"
  )

  # Result with an explicitly provided join_crs
  res_join_crs <- dst_interpolate_aw(
    target = wards,
    tid = WARD,
    source = race_wgs84,
    sid = GEOID,
    extensive = "TOTAL_E",
    join_crs = 3857 # Web Mercator
  )

  testthat::expect_equal(
    res_native_crs$TOTAL_E,
    res_diff_crs$TOTAL_E,
    tolerance = 1e-6
  )
  testthat::expect_equal(
    res_native_crs$TOTAL_E,
    res_join_crs$TOTAL_E,
    tolerance = 1e-6
  )
})
