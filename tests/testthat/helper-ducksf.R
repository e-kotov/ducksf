# testthat helper (edition 3 recommended)
# usethis::use_testthat(3)

# Tolerances (tweak if your magnitudes are huge)
.ducksf_rtol <- 1e-8
.ducksf_atol <- 1e-6

almost_equal <- function(a, b, rtol = .ducksf_rtol, atol = .ducksf_atol) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  d <- abs(a - b)
  m <- pmax(abs(a), abs(b))
  d <= (atol + rtol * m)
}

max_abs_diff <- function(a, b) {
  max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE)
}

sum_numeric <- function(x) sum(as.numeric(x), na.rm = TRUE)
