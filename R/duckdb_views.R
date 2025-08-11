#' Project a view's geometry into a new view (or pass-through if CRS matches)
#' @keywords internal
ducksf_project_view <- function(
  con,
  in_view,
  out_view,
  from_crs_lit,
  to_crs_lit,
  same_crs = FALSE,
  geom_col = "geom"
) {
  iv <- DBI::dbQuoteIdentifier(con, in_view)
  ov <- DBI::dbQuoteIdentifier(con, out_view)
  gc <- DBI::dbQuoteIdentifier(con, geom_col)

  if (isTRUE(same_crs)) {
    DBI::dbExecute(
      con,
      glue::glue("CREATE OR REPLACE VIEW {ov} AS SELECT * FROM {iv};")
    )
  } else {
    # DuckDB supports SELECT * REPLACE(...)
    DBI::dbExecute(
      con,
      glue::glue(
        "
        CREATE OR REPLACE VIEW {ov} AS
        SELECT * REPLACE (ST_Transform({gc}, {from_crs_lit}, {to_crs_lit}) AS {gc})
        FROM {iv};
      "
      )
    )
  }
  out_view
}

#' Normalize a raw table into <id, geom, ...vars> with canonical names
#' @keywords internal
ducksf_create_norm_view <- function(
  con,
  raw_tbl,
  out_view,
  id_col, # name in the raw table
  out_id = c("sid", "tid"),
  geom_col_raw = "geometry",
  geom_alias = "geom",
  keep_cols = character(0)
) {
  out_id <- match.arg(out_id)
  rv <- DBI::dbQuoteIdentifier(con, raw_tbl)
  ov <- DBI::dbQuoteIdentifier(con, out_view)

  idq <- DBI::dbQuoteIdentifier(con, id_col)
  gq <- DBI::dbQuoteIdentifier(con, geom_col_raw)
  galias <- DBI::dbQuoteIdentifier(con, geom_alias)

  extra <- if (length(keep_cols)) {
    paste0(", ", paste(DBI::dbQuoteIdentifier(con, keep_cols), collapse = ", "))
  } else {
    ""
  }

  DBI::dbExecute(
    con,
    glue::glue(
      "
      CREATE OR REPLACE VIEW {ov} AS
      SELECT
        {idq} AS {DBI::dbQuoteIdentifier(con, out_id)},
        {gq}  AS {galias}
        {extra}
      FROM {rv};
    "
    )
  )
  out_view
}

#' Build normalized + (optionally) projected source/target views
#' @keywords internal
ducksf_build_proj_views <- function(
  con,
  sidQN,
  tidQN,
  src_lit,
  trg_lit,
  join_lit,
  same_src_join,
  same_trg_join,
  vars = character(0),
  source_tbl_raw = "source_tbl_raw",
  target_tbl_raw = "target_tbl_raw",
  src_geom_name = "geometry",
  trg_geom_name = "geometry"
) {
  # 1) Normalize raw tables -> canonical views with id=<sid|tid>, geom=<geom>
  ducksf_create_norm_view(
    con,
    source_tbl_raw,
    "source_tbl",
    id_col = sidQN,
    out_id = "sid",
    geom_col_raw = src_geom_name,
    geom_alias = "geom",
    keep_cols = vars
  )
  ducksf_create_norm_view(
    con,
    target_tbl_raw,
    "target_tbl",
    id_col = tidQN,
    out_id = "tid",
    geom_col_raw = trg_geom_name,
    geom_alias = "geom",
    keep_cols = character(0)
  )

  # 2) Project each to join CRS (or pass-through)
  ducksf_project_view(
    con,
    "source_tbl",
    "source_proj",
    from_crs_lit = src_lit,
    to_crs_lit = join_lit,
    same_crs = same_src_join,
    geom_col = "geom"
  )
  ducksf_project_view(
    con,
    "target_tbl",
    "target_proj",
    from_crs_lit = trg_lit,
    to_crs_lit = join_lit,
    same_crs = same_trg_join,
    geom_col = "geom"
  )

  invisible(list(
    source_tbl = "source_tbl",
    source_proj = "source_proj",
    target_tbl = "target_tbl",
    target_proj = "target_proj"
  ))
}
