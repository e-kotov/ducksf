#' Enable or disable preferring ducksf replacements over sf (no attach)
#'
#' Since ducksf does **not export** any functions with the same names as `sf`,
#' there is nothing to prefer/mask at runtime. This helper is kept only for
#' backward compatibility and will just inform the user.
#'
#' @param use   Ignored (kept for compatibility).
#' @param allow Ignored.
#' @param deny  Ignored.
#' @param silent Logical; if TRUE, suppress messages.
#' @return Invisibly returns character(0).
#' @export
sf_use_ducksf <- function(
  use = NULL,
  allow = NULL,
  deny = NULL,
  silent = FALSE
) {
  if (!isTRUE(silent)) {
    message(
      "[ducksf] No masking of `sf` is performed because ducksf does not export\n",
      "conflicting names. Call explicitly:\n",
      "  - ducksf::st_interpolate_aw_ducksf()  # sf-compatible wrapper (no masking)\n",
      "  - ducksf::dst_interpolate_aw()      # fast path API\n"
    )
  }
  invisible(character(0))
}

.ducksf_override_registry <- new.env(parent = emptyenv())
.ducksf_register_override <- function(name, fun) {
  assign(name, fun, envir = .ducksf_override_registry)
}
.ducksf_registry_names <- function() {
  ls(.ducksf_override_registry, all.names = FALSE)
}

# Internal environment holding the functions we *can* override (names only)
.ducksf_override_registry <- new.env(parent = emptyenv())

# Register a function name that can be preference-toggled
.ducksf_register_override <- function(name, fun) {
  # We only need the name; keeping a handle allows future extensions if needed.
  assign(name, fun, envir = .ducksf_override_registry)
}

# List registered overrideable names
.ducksf_registry_names <- function() {
  ls(.ducksf_override_registry, all.names = FALSE)
}
