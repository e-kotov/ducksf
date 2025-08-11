test_that("ducksf_interpolate_aw_plan_vars drops geometry and handles NA names", {
  sf <- sf::st_as_sf(data.frame(a = 1:2, b = 3:4), coords = c("a", "b"))
  sf$val <- c(1, 2)
  pv <- ducksf_interpolate_aw_plan_vars(sf, extensive = c("val", NA))
  expect_equal(pv$ex_vars, "val")
  expect_equal(pv$type, "extensive")
})
