#' Build denominator table/view for areal/length/count kernels
#' @keywords internal
ducksf_build_denom <- function(
  con,
  by = c("sid", "tid"),
  mode = NULL, # "sum" or "total" (only relevant for by="sid")
  measure = c("area", "length", "count"),
  overlap_view = "overlap",
  src_proj_view = "source_proj",
  out_view = NULL, # defaults to "total_by_<by>"
  id_col = NULL, # defaults to by
  alias = NULL # defaults to "total_<measure>_<by>"
) {
  by <- match.arg(by)
  measure <- match.arg(measure)
  mode <- if (is.null(mode)) {
    if (by == "tid") "sum" else "sum"
  } else {
    match.arg(mode, c("sum", "total"))
  }

  # defaults for names
  out_view <- out_view %||% paste0("total_by_", by)
  id_col <- id_col %||% by
  alias <- alias %||% paste0("total_", measure, "_", by)

  ov <- DBI::dbQuoteIdentifier(con, overlap_view)
  sp <- DBI::dbQuoteIdentifier(con, src_proj_view)
  out <- DBI::dbQuoteIdentifier(con, out_view)
  idq <- DBI::dbQuoteIdentifier(con, id_col)
  alia <- DBI::dbQuoteIdentifier(con, alias)

  # overlap measure column name
  ov_col <- switch(
    measure,
    area = "overlap_area",
    length = "overlap_length",
    count = "overlap_count"
  )
  ovq <- DBI::dbQuoteIdentifier(con, ov_col)

  if (by == "tid") {
    if (mode == "total") {
      stop("mode='total' is not defined for by='tid'. Use mode='sum'.")
    }
    DBI::dbExecute(
      con,
      glue::glue(
        "
      CREATE OR REPLACE VIEW {out} AS
      SELECT {idq} AS {idq}, SUM({ovq}) AS {alia}
      FROM {ov}
      GROUP BY {idq};
    "
      )
    )
  } else {
    # by == "sid"
    if (mode == "sum") {
      DBI::dbExecute(
        con,
        glue::glue(
          "
        CREATE OR REPLACE VIEW {out} AS
        SELECT {idq} AS {idq}, SUM({ovq}) AS {alia}
        FROM {ov}
        GROUP BY {idq};
      "
        )
      )
    } else {
      # mode == "total"
      if (measure == "count") {
        stop("mode='total' is not defined for measure='count'.")
      }
      geom_expr <- if (measure == "area") "ST_Area(geom)" else "ST_Length(geom)"
      DBI::dbExecute(
        con,
        glue::glue(
          "
        CREATE OR REPLACE VIEW {out} AS
        SELECT {idq} AS {idq}, {geom_expr} AS {alia}
        FROM {sp};
      "
        )
      )
    }
  }

  invisible(list(
    out_view = out_view,
    alias = alias,
    by = by,
    measure = measure,
    mode = mode
  ))
}
