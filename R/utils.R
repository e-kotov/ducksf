#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @keywords internal
.ducksf_resolve_crs <- function(crs) {
  if (inherits(crs, "crs")) {
    epsg <- crs$epsg
    if (!is.na(epsg)) {
      return(epsg)
    }
    if (!is.null(crs$wkt)) {
      return(crs$wkt)
    }
    return(NULL)
  }
  crs
}

#' @keywords internal
.ducksf_crs_literal <- function(crs) {
  if (is.numeric(crs)) {
    paste0("'EPSG:", as.integer(crs), "'")
  } else if (is.character(crs)) {
    paste0("'", gsub("'", "''", crs), "'")
  } else {
    stop("Unsupported CRS literal.")
  }
}

#' @keywords internal
.ducksf_crs_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) {
    return(FALSE)
  }
  if (is.numeric(a) && is.numeric(b)) {
    return(as.integer(a) == as.integer(b))
  }
  norm <- function(x) {
    if (is.numeric(x)) {
      paste0("EPSG:", as.integer(x))
    } else {
      toupper(trimws(as.character(x)))
    }
  }
  norm(a) == norm(b)
}

#' @keywords internal
.ducksf_register_sf <- function(con, obj, view_name, overwrite = FALSE) {
  df <- sf::st_drop_geometry(obj)
  wkb <- wk::as_wkb(sf::st_geometry(obj)) # <- instead of sf::st_as_binary(...)
  df$geometry <- geoarrow::as_geoarrow_vctr(
    wkb,
    schema = geoarrow::geoarrow_wkb()
  )
  arr <- arrow::Table$create(df)
  if (overwrite && duckdb::dbExistsTable(con, view_name)) {
    duckdb::duckdb_unregister_arrow(con, view_name)
  }
  duckdb::duckdb_register_arrow(con, view_name, arr)
  invisible(view_name)
}

#' @keywords internal
.ducksf_normalize_cols <- function(expr, name, env = rlang::caller_env()) {
  # bare: tid = GEOID
  if (rlang::is_symbol(expr)) {
    return(rlang::as_string(expr))
  }
  # string literal: tid = "GEOID"
  if (rlang::is_string(expr) && length(expr) == 1L) {
    return(as.character(expr))
  }
  # variable holding a string: my_id <- "GEOID"; tid = my_id
  val <- rlang::eval_bare(expr, env)
  if (is.character(val) && length(val) == 1L) {
    return(val)
  }

  stop(glue::glue("`{name}` must be a bare column name or a single string."))
}
