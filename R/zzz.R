.onAttach <- function(...) {
  packageStartupMessage(
    "[ducksf] No masking of {sf}. Use ducksf::st_interpolate_aw_ducksf() ",
    "or ducksf::dst_interpolate_aw()."
  )
}
