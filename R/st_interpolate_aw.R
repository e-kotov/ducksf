#' Areal-weighted interpolation via DuckDB (no masking)
#'
#' @name st_interpolate_aw_ducksf
#' @title Areal-weighted interpolation via DuckDB (no masking)
#'
#' @description
#' `st_interpolate_aw_ducksf()` is a user-facing, **non-masking** wrapper that
#' reproduces the semantics of `sf::st_interpolate_aw()` while delegating work
#' to [`dst_interpolate_aw()`]. It never masks \code{sf}; call it explicitly.
#'
#' There is also an **internal** shim, `st_interpolate_aw()` (unexported),
#' with the same signature. It exists so that a future, optional
#' preference mechanism (e.g. via the **conflicted** package) could prefer
#' ducksf’s implementation without changing the search path. In this package
#' version, no masking is performed.
#'
#' @param x An `sf` object with source polygons and attributes to interpolate.
#' @param to An `sf` or `sfc` with target polygons (coerced via `sf::st_as_sf()` if needed).
#' @param extensive Either a single logical (sf semantics) or a character vector of
#'   column names to treat as spatially extensive (package extension).
#' @param ... Ignored; present for drop-in compatibility with `sf`.
#' @param keep_NA Logical; if `FALSE`, drop targets with no contributions in the
#'   requested variables (matches sf).
#' @param na.rm Logical; if `TRUE`, drop rows of `x` that contain any `NA` before
#'   interpolation (matches sf).
#'
#' @return An `sf` object with interpolated variables merged to `to`.
#'
#' @details
#' - **CRS** is inferred from inputs and passed to DuckDB as EPSG/WKT/JSON; for
#'   area work, use a projected CRS via `join_crs` in [`dst_interpolate_aw()`].
#' - **Weights mapping (sf semantics):** if any variables are treated as *extensive*,
#'   the shim uses `weight = "total"`; otherwise `weight = "sum"`.
#'
#' @seealso
#'   - `sf::st_interpolate_aw()` for the original,
#'   - [`dst_interpolate_aw()`] for the fast-path API.
#'
#' @examples
#' \dontrun{
#' # Explicit, no masking:
#' res <- ducksf::st_interpolate_aw_ducksf(x = src_sf, to = tgt_sf, extensive = TRUE)
#'
#' # Original sf still available:
#' res_sf <- sf::st_interpolate_aw(x = src_sf, to = tgt_sf, extensive = TRUE)
#' }
#'
#' @export
st_interpolate_aw_ducksf <- function(
  x,
  to,
  extensive,
  ...,
  keep_NA = FALSE,
  na.rm = FALSE
) {
  st_interpolate_aw(
    x = x,
    to = to,
    extensive = extensive,
    ...,
    keep_NA = keep_NA,
    na.rm = na.rm
  )
}

#' @rdname st_interpolate_aw_ducksf
#' @aliases st_interpolate_aw
#' @keywords internal
st_interpolate_aw <- function(
  x,
  to,
  extensive,
  ...,
  keep_NA = FALSE, # sf default
  na.rm = FALSE # sf default
) {
  # 1) Coerce `to` like sf does
  if (!inherits(to, "sf") && !inherits(to, "sfc")) {
    to <- try(sf::st_as_sf(to), silent = TRUE)
    if (inherits(to, "try-error")) {
      stop("st_interpolate_aw requires geometries in argument `to`")
    }
  }

  # 2) sf-style variable selection:
  nx <- sf::st_drop_geometry(x)
  num_cols <- names(nx)[vapply(nx, is.numeric, TRUE)]
  if (missing(extensive)) {
    stop("argument `extensive` is missing, with no default (TRUE/FALSE).")
  }

  if (is.logical(extensive) && length(extensive) == 1L) {
    if (extensive) {
      ex_vars <- num_cols
      in_vars <- character(0)
    } else {
      ex_vars <- character(0)
      in_vars <- num_cols
    }
  } else if (is.character(extensive)) {
    ex_vars <- extensive
    in_vars <- character(0)
  } else {
    stop(
      "`extensive` must be TRUE/FALSE or a character vector of column names."
    )
  }

  # 3) sf-style na.rm
  if (isTRUE(na.rm)) {
    keep_rows <- !apply(is.na(sf::st_drop_geometry(x)), 1L, any)
    x <- x[keep_rows, , drop = FALSE]
  }

  # 4) Temp IDs
  if (!".__sid__" %in% names(x)) {
    x$.__sid__ <- seq_len(nrow(x))
  }
  if (!".__tid__" %in% names(to)) {
    to$.__tid__ <- seq_len(nrow(to))
  }

  # 5) Call core with sf-consistent weight mapping
  weight_mode <- if (length(ex_vars)) "total" else "sum"

  out <- dst_interpolate_aw(
    target_sf = to,
    tid = ".__tid__",
    source_sf = x,
    sid = ".__sid__",
    weight = weight_mode,
    output = "sf",
    extensive = ex_vars,
    intensive = in_vars,
    join_crs = NULL,
    duckdb_threads = getOption("ducksf.aw.threads", NULL),
    na.rm = FALSE,
    keep_NA = keep_NA
  )

  if (!isTRUE(keep_NA) && ".__tid__" %in% names(out)) {
    row.names(out) <- out$.__tid__
  }
  out$.__tid__ <- NULL
  out
}
