#' Make sf-ready view (tid + geometry + interpolated cols)
#' @keywords internal
ducksf_build_interpolated_sf_view <- function(
  con,
  target_tbl = "target_tbl",
  ia_view = "interpolated_all",
  out_view = "interpolated_sf"
) {
  tt <- DBI::dbQuoteIdentifier(con, target_tbl)
  ia <- DBI::dbQuoteIdentifier(con, ia_view)
  out <- DBI::dbQuoteIdentifier(con, out_view)
  DBI::dbExecute(
    con,
    glue::glue(
      "
    CREATE OR REPLACE VIEW {out} AS
    SELECT t.tid, t.geom AS geometry, ia.*
    FROM {tt} t
    LEFT JOIN {ia} ia USING (tid);
  "
    )
  )
  invisible(out_view)
}

#' Collect result as sf or tibble, mirroring current behavior
#' @keywords internal
ducksf_collect_output <- function(
  con,
  output = c("sf", "tibble"),
  target_sf,
  target_crs,
  tidQN,
  nameConflict = FALSE,
  tidOrig = NULL,
  ia_view = "interpolated_all",
  isf_view = "interpolated_sf"
) {
  output <- match.arg(output)
  if (output == "sf") {
    x <- dplyr::tbl(con, isf_view) |> arrow::to_arrow()
    tmp <- sf::st_as_sf(x, crs = target_crs)
    out <- dplyr::left_join(
      target_sf,
      dplyr::as_tibble(tmp),
      by = dplyr::join_by(!!tidQN == "tid")
    )
  } else {
    x <- dplyr::tbl(con, ia_view) |> arrow::to_arrow()
    out <- dplyr::as_tibble(x)
    tgt_id <- dplyr::select(
      sf::st_drop_geometry(target_sf),
      !!rlang::sym(tidQN)
    )
    out <- dplyr::left_join(tgt_id, out, by = dplyr::join_by(!!tidQN == "tid"))
  }
  if (isTRUE(nameConflict) && !is.null(tidOrig)) {
    out <- dplyr::rename(out, !!rlang::set_names(rlang::sym(tidQN), tidOrig))
  }
  out
}

#' Apply keep_NA semantics (drop targets with all-NA on requested vars)
#' @keywords internal
ducksf_apply_keep_na <- function(
  out,
  output = c("sf", "tibble"),
  vars_used,
  keep_NA
) {
  output <- match.arg(output)
  if (isTRUE(keep_NA) || !length(vars_used)) {
    return(out)
  }
  if (output == "sf") {
    df <- sf::st_drop_geometry(out)
    keep_t <- rowSums(!is.na(df[, vars_used, drop = FALSE])) > 0
    out[keep_t, , drop = FALSE]
  } else {
    keep_t <- rowSums(!is.na(out[, vars_used, drop = FALSE])) > 0
    out[keep_t, , drop = FALSE]
  }
}
