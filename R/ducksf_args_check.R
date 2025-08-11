# R/duckdb_plan.R

#' Handle sid/tid same-name conflict on the target
#' @keywords internal
ducksf_handle_id_conflict <- function(target_sf, tidQN, sidQN) {
  nameConflict <- FALSE
  tidOrig <- tidQN
  if (identical(tidQN, sidQN)) {
    target_sf <- dplyr::rename(target_sf, ...tid = !!rlang::sym(tidQN))
    tidQN <- "...tid"
    nameConflict <- TRUE
  }
  list(
    target_sf = target_sf,
    tidQN = tidQN,
    nameConflict = nameConflict,
    tidOrig = tidOrig
  )
}


#' Validate/select variables, enforce numeric, apply na.rm, infer type
#' @keywords internal
ducksf_interpolate_aw_plan_vars <- function(
  source_sf,
  extensive,
  intensive,
  na.rm = FALSE
) {
  if (missing(extensive) && missing(intensive)) {
    stop("Specify 'extensive' and/or 'intensive'.")
  }

  # Coerce to clean character vectors (remove NA/empty, trim spaces)
  ex_vars <- if (missing(extensive)) character(0) else as.character(extensive)
  in_vars <- if (missing(intensive)) character(0) else as.character(intensive)
  clean <- function(x) unique(stats::na.omit(trimws(x)))
  ex_vars <- clean(ex_vars)
  in_vars <- clean(in_vars)

  # Determine type
  type <- if (length(ex_vars) && !length(in_vars)) {
    "extensive"
  } else if (!length(ex_vars) && length(in_vars)) {
    "intensive"
  } else {
    "mixed"
  }

  vars_used <- unique(c(ex_vars, in_vars))

  # Work on non-spatial data only
  nx <- sf::st_drop_geometry(source_sf)

  # Presence check (better message than letting subsetting fail)
  missing_cols <- setdiff(vars_used, names(nx))
  if (length(missing_cols)) {
    stop(
      "Requested variable(s) not found in source_sf: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Numeric type check on the requested columns only
  if (length(vars_used)) {
    bad <- vars_used[!vapply(nx[vars_used], is.numeric, logical(1))]
    if (length(bad)) {
      stop(
        "Non-numeric variable(s) in 'source_sf': ",
        paste(bad, collapse = ", "),
        ". Only numeric columns can be interpolated."
      )
    }
    if (isTRUE(na.rm)) {
      keep_rows <- rowSums(is.na(nx[vars_used])) == 0L
      source_sf <- source_sf[keep_rows, , drop = FALSE]
    }
  }

  list(
    type = type,
    ex_vars = ex_vars,
    in_vars = in_vars,
    vars_used = vars_used,
    source_sf = source_sf
  )
}

#' Assert required columns exist (simple, reusable)
#' @keywords internal
ducksf_check_cols_present <- function(df, cols, where = "data frame") {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop("Missing column(s) in ", where, ": ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}
