#' Build a skinny overlap relation between projected source/target views
#' @keywords internal
ducksf_build_overlap <- function(
  con,
  src_view = "source_proj",
  trg_view = "target_proj",
  out_view = "overlap",
  join_pred = "intersects",
  measure = c("area", "length", "count"),
  keep_geom = FALSE,
  materialize = FALSE,
  # extras
  dwithin_distance = NULL, # numeric; units of join_crs
  prefilter_bbox = TRUE, # add ST_Intersects_Extent(...) AND ...
  src_id_col = "sid",
  trg_id_col = "tid",
  geom_col_name = "geom"
) {
  measure <- match.arg(measure)

  ivs <- DBI::dbQuoteIdentifier(con, src_view)
  ivt <- DBI::dbQuoteIdentifier(con, trg_view)
  ovo <- DBI::dbQuoteIdentifier(con, out_view)
  sid <- DBI::dbQuoteIdentifier(con, src_id_col)
  tid <- DBI::dbQuoteIdentifier(con, trg_id_col)

  norm <- gsub("[^a-z]", "", tolower(join_pred))
  pred_sql <- switch(
    norm,
    "intersects" = "ST_Intersects(t.geom, s.geom)",
    "contains" = "ST_Contains(t.geom, s.geom)",
    "containsproperly" = "ST_ContainsProperly(t.geom, s.geom)",
    "within" = "ST_Within(t.geom, s.geom)",
    "withinproperly" = "ST_WithinProperly(t.geom, s.geom)",
    "overlaps" = "ST_Overlaps(t.geom, s.geom)",
    "touches" = "ST_Touches(t.geom, s.geom)",
    "crosses" = "ST_Crosses(t.geom, s.geom)",
    "equals" = "ST_Equals(t.geom, s.geom)",
    "covers" = "ST_Covers(t.geom, s.geom)",
    "coveredby" = "ST_CoveredBy(t.geom, s.geom)",
    "dwithin" = {
      if (
        is.null(dwithin_distance) ||
          !is.numeric(dwithin_distance) ||
          length(dwithin_distance) != 1L
      ) {
        stop(
          "join_pred='dwithin' requires numeric dwithin_distance (units of join_crs)."
        )
      }
      paste0(
        "ST_DWithin(t.geom, s.geom, ",
        format(dwithin_distance, scientific = FALSE),
        ")"
      )
    },
    # deliberately not supported in this kernel (anti-join)
    "disjoint" = stop(
      "join_pred='disjoint' is not supported in overlap builder (anti-join)."
    ),
    stop(
      "Unsupported join_predicate: ",
      join_pred,
      ". Try: intersects, contains, contains_properly, within, within_properly, ",
      "overlaps, touches, crosses, equals, covers, covered_by, dwithin."
    )
  )

  # if bbox prefilter, wrap both sides with bbox columns and add numeric range join
  if (isTRUE(prefilter_bbox) && norm != "dwithin") {
    src_bbox <- paste0(src_view, "_bbox")
    trg_bbox <- paste0(trg_view, "_bbox")
    ducksf_add_bbox_columns(con, src_view, src_bbox, geom_col_name)
    ducksf_add_bbox_columns(con, trg_view, trg_bbox, geom_col_name)

    ivs <- DBI::dbQuoteIdentifier(con, src_bbox)
    ivt <- DBI::dbQuoteIdentifier(con, trg_bbox)

    bbox_terms <- c(
      "t.minx <= s.maxx",
      "t.maxx >= s.minx",
      "t.miny <= s.maxy",
      "t.maxy >= s.miny"
    )
    bbox_sql <- paste(bbox_terms, collapse = " AND ")
    pred_sql <- paste0("(", bbox_sql, ") AND (", pred_sql, ")")
  }

  mea <- switch(
    measure,
    area = list(
      expr = "COALESCE(ST_Area(ST_Intersection(t.geom, s.geom)), 0)",
      alias = "overlap_area"
    ),
    length = list(
      expr = "COALESCE(ST_Length(ST_Intersection(t.geom, s.geom)), 0)",
      alias = "overlap_length"
    ),
    count = list(expr = "1::DOUBLE", alias = "overlap_count")
  )

  geom_sel <- if (isTRUE(keep_geom)) {
    ", ST_Intersection(t.geom, s.geom) AS geom"
  } else {
    ""
  }
  create_kw <- if (isTRUE(materialize)) {
    "CREATE OR REPLACE TEMPORARY TABLE"
  } else {
    "CREATE OR REPLACE VIEW"
  }

  DBI::dbExecute(
    con,
    glue::glue(
      "
      {create_kw} {ovo} AS
      SELECT s.{sid} AS sid,
             t.{tid} AS tid,
             {mea$expr} AS {DBI::dbQuoteIdentifier(con, mea$alias)}{geom_sel}
      FROM {ivt} t
      JOIN {ivs} s
        ON {pred_sql};
    "
    )
  )

  invisible(list(
    overlap = out_view,
    measure_col = mea$alias,
    geom_col = if (isTRUE(keep_geom)) "geom" else NULL
  ))
}

#' @keywords internal
ducksf_add_bbox_columns <- function(con, in_view, out_view, geom_col = "geom") {
  iv <- DBI::dbQuoteIdentifier(con, in_view)
  ov <- DBI::dbQuoteIdentifier(con, out_view)
  gc <- DBI::dbQuoteIdentifier(con, geom_col)
  DBI::dbExecute(
    con,
    glue::glue(
      "
      CREATE OR REPLACE VIEW {ov} AS
      SELECT
        *,
        ST_XMin({gc}) AS minx,
        ST_XMax({gc}) AS maxx,
        ST_YMin({gc}) AS miny,
        ST_YMax({gc}) AS maxy
      FROM {iv};
    "
    )
  )
  out_view
}
