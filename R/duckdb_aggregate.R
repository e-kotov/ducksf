#' Compile aggregation view for AW/IW (area/length/count kernels)
#' @keywords internal
ducksf_compile_aggs <- function(
  con,
  ex_vars = character(0),
  in_vars = character(0),
  measure = c("area", "length", "count"),
  overlap_view = "overlap",
  source_proj_view = "source_proj",
  denom_sid_view = "total_by_sid",
  denom_tid_view = "total_by_tid",
  denom_sid_alias = NULL, # defaults to total_<measure>_sid
  denom_tid_alias = NULL, # defaults to total_<measure>_tid
  out_view = "interpolated_all",
  tid_col = "tid" # id on the overlap/target side
) {
  measure <- match.arg(measure)
  overlap_col <- switch(
    measure,
    area = "overlap_area",
    length = "overlap_length",
    count = "overlap_count"
  )
  denom_sid_alias <- denom_sid_alias %||% paste0("total_", measure, "_sid")
  denom_tid_alias <- denom_tid_alias %||% paste0("total_", measure, "_tid")

  ov <- DBI::dbQuoteIdentifier(con, overlap_view)
  sp <- DBI::dbQuoteIdentifier(con, source_proj_view)
  tbs <- DBI::dbQuoteIdentifier(con, denom_sid_view)
  tbt <- DBI::dbQuoteIdentifier(con, denom_tid_view)
  out <- DBI::dbQuoteIdentifier(con, out_view)

  tidq <- DBI::dbQuoteIdentifier(con, tid_col)
  ovq <- DBI::dbQuoteIdentifier(con, overlap_col)
  den_sid_q <- DBI::dbQuoteIdentifier(con, denom_sid_alias)
  den_tid_q <- DBI::dbQuoteIdentifier(con, denom_tid_alias)

  # Build SELECT expressions
  ex_exprs <- character(0)
  if (length(ex_vars)) {
    ex_exprs <- paste0(
      "SUM((src.",
      DBI::dbQuoteIdentifier(con, ex_vars),
      ") * o.",
      ovq,
      " / NULLIF(tbs.",
      den_sid_q,
      ", 0)) AS ",
      DBI::dbQuoteIdentifier(con, ex_vars)
    )
  }

  in_exprs <- character(0)
  if (length(in_vars)) {
    in_exprs <- paste0(
      "SUM((src.",
      DBI::dbQuoteIdentifier(con, in_vars),
      ") * o.",
      ovq,
      " / NULLIF(tbt.",
      den_tid_q,
      ", 0)) AS ",
      DBI::dbQuoteIdentifier(con, in_vars)
    )
  }

  select_expr <- paste(c(ex_exprs, in_exprs), collapse = ",\n      ")

  # Conditional joins
  join_src <- if (length(c(ex_vars, in_vars)) > 0) {
    paste("JOIN", sp, "src USING (sid)")
  } else {
    ""
  }
  join_sid <- if (length(ex_vars) > 0) {
    paste("LEFT JOIN", tbs, "tbs USING (sid)")
  } else {
    ""
  }
  join_tid <- if (length(in_vars) > 0) {
    paste("LEFT JOIN", tbt, "tbt USING (tid)")
  } else {
    ""
  }

  sql <- glue::glue(
    "
    CREATE OR REPLACE VIEW {out} AS
    SELECT
      o.{tidq}
      { if (nzchar(select_expr)) paste0(',\n      ', select_expr) else '' }
    FROM {ov} o
    {join_src}
    {join_sid}
    {join_tid}
    GROUP BY o.{tidq};
  "
  )

  DBI::dbExecute(con, sql)
  invisible(out_view)
}
