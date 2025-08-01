#' Areal-weighted interpolation of polygon data (DuckDB-backed)
#'
#' @description
#' A fast, DuckDB Spatial-backed reimplementation of areal-weighted interpolation found in \code{\link[areal:aw_interpolate]{areal::aw_interpolate()}} and \code{\link[sf:st_interpolate_aw]{sf::st_interpolate_aw()}}. It performs all calculations using a temporary in-memory DuckDB database.
#'
#' @usage
#' dst_interpolate_aw(
#'   target_sf, tid, source_sf, sid, weight = "sum", output = "sf",
#'   extensive, intensive, source_crs = NULL, target_crs = NULL, join_crs = NULL,
#'   duckdb_threads = NULL, na.rm = FALSE, keep_NA = TRUE
#' )
#'
#' @details
#' Areal-weighted interpolation estimates values for overlapping but non-congruent polygons and assumes the attribute is evenly distributed across each source polygon. Results are most reliable when a projected CRS is used for area calculations.
#'
#' **CRS handling.** DuckDB does not persist CRS metadata. CRS values used here are **inferred from the provided \code{sf} objects** via \code{sf::st_crs()} and then passed to DuckDB Spatial as EPSG/WKT/PROJJSON literals for transformations. If \code{source_crs}/\code{target_crs} are not supplied, they default to the CRS of the corresponding \code{sf} objects. \code{join_crs} defaults to \code{target_crs}. For area-based workflows, supply a **projected** \code{join_crs}.
#'
#' **Weights.** For spatially extensive variables, \code{weight = "sum"} uses the *intersected* area per source feature as the denominator; \code{weight = "total"} uses the full source polygon area. For spatially intensive variables, denominators are computed per target using the sum of intersected areas and \code{weight = "sum"}.
#'
#' @param target_sf A \code{sf} object with polygons to which values are interpolated
#'   (the target geometry).
#' @param tid A column in \code{target_sf} that uniquely identifies target features.
#' @param source_sf A \code{sf} object with polygons providing the values to interpolate
#'   (the source geometry).
#' @param sid A column in \code{source_sf} that uniquely identifies source features.
#' @param weight For spatially extensive variables, either \code{"sum"} (default) or
#'   \code{"total"}. For spatially intensive variables, use \code{"sum"}.
#' @param output One of \code{"sf"} (default) or \code{"tibble"} for the return type.
#' @param extensive Character vector of column names in \code{source_sf} treated as
#'   spatially extensive (e.g., counts). Optional if \code{intensive} is supplied.
#' @param intensive Character vector of column names in \code{source_sf} treated as
#'   spatially intensive (e.g., rates/densities). Optional if \code{extensive} is supplied.
#' @param source_crs,target_crs Optional CRS specifications for the source/target inputs.
#'   If omitted, they are inferred from \code{sf::st_crs(source_sf)} and
#'   \code{sf::st_crs(target_sf)} respectively. Accepts EPSG code (numeric) or
#'   WKT/PROJJSON (character).
#' @param join_crs Optional CRS used for the spatial join and area calculations.
#'   If omitted, defaults to \code{target_crs}. **Use a projected CRS** for area-based work.
#' @param duckdb_threads Optional integer: number of DuckDB threads. If \code{NULL}, the
#'   DuckDB default is used.
#' @param na.rm Logical; if \code{TRUE}, drops rows from \code{source_sf} with any \code{NA}
#'   in the variables being interpolated. Defaults to \code{FALSE}.
#' @param keep_NA Logical; if \code{FALSE}, drops targets where all requested interpolated
#'   variables are \code{NA}. Defaults to \code{TRUE}.
#'
#' @return An \code{sf} object (default) with interpolated variables merged to
#'   \code{target_sf}, or a \code{tibble} when \code{output = "tibble"}.
#'
#' @examples
#' \dontrun{
#' # Sketch (using your own sf objects):
#' res <- dst_interpolate_aw(
#'   target_sf = target,
#'   tid       = target_id,
#'   source_sf = source,
#'   sid       = source_id,
#'   weight    = "sum",
#'   extensive = c("pop_total", "jobs"),
#'   intensive = "density",
#'   join_crs  = 3857
#' )
#' }
#'
#' @seealso
#' \code{\link[areal:aw_interpolate]{areal::aw_interpolate()}} — reference implementation.
#'
#' @references
#' Prener, C. and Revord, C. (2019). \emph{areal: An R package for areal weighted interpolation}.
#' \emph{Journal of Open Source Software}, 4(37), 1221.
#' Available at: \url{https://doi.org/10.21105/joss.01221}
#'
#' @export
dst_interpolate_aw <- function(
  target_sf,
  tid,
  source_sf,
  sid,
  weight = "sum",
  output = "sf",
  extensive,
  intensive,
  source_crs = NULL,
  target_crs = NULL,
  join_crs = NULL,
  duckdb_threads = NULL,
  na.rm = FALSE,
  keep_NA = TRUE
) {
  # ---- checks (unchanged) ----
  if (missing(target_sf)) {
    stop("Provide 'target_sf'.")
  }
  if (missing(source_sf)) {
    stop("Provide 'source_sf'.")
  }
  if (missing(tid)) {
    stop("Provide 'tid'.")
  }
  if (missing(sid)) {
    stop("Provide 'sid'.")
  }
  if (missing(extensive) && missing(intensive)) {
    stop("Specify 'extensive' and/or 'intensive'.")
  }
  if (!weight %in% c("sum", "total")) {
    stop("weight must be 'sum' or 'total'.")
  }
  if (!output %in% c("sf", "tibble")) {
    stop("output must be 'sf' or 'tibble'.")
  }

  # ---- ids: accept bare names or strings, then canonicalize ----
  sidQN <- .ducksf_normalize_cols(rlang::enexpr(sid), "sid")
  tidQN <- .ducksf_normalize_cols(rlang::enexpr(tid), "tid")

  if (!sidQN %in% names(source_sf)) {
    stop(glue::glue(
      "Column '{sidQN}' (sid) not found in source_sf. Available: {paste(names(source_sf), collapse = ', ')}"
    ))
  }

  if (!tidQN %in% names(target_sf)) {
    stop(glue::glue(
      "Column '{tidQN}' (tid) not found in target_sf. Available: {paste(names(target_sf), collapse = ', ')}"
    ))
  }

  # Symbols only when needed for dplyr calls:
  sid_sym <- rlang::sym(sidQN)
  tid_sym <- rlang::sym(tidQN)

  # conflict (unchanged)
  nameConflict <- FALSE
  tidOrig <- tidQN
  if (identical(tidQN, sidQN)) {
    target_sf <- dplyr::rename(target_sf, ...tid = !!tid_sym)
    tidQN <- "...tid"
    tid_sym <- rlang::sym(tidQN)
    nameConflict <- TRUE
  }

  # variables (unchanged)
  if (missing(intensive) && !missing(extensive)) {
    type <- "extensive"
    ex_vars <- extensive
    in_vars <- character(0)
  } else if (!missing(intensive) && missing(extensive)) {
    if (weight == "total") {
      stop("Intensive requires weight='sum'.")
    }
    type <- "intensive"
    ex_vars <- character(0)
    in_vars <- intensive
  } else {
    type <- "mixed"
    ex_vars <- extensive
    in_vars <- intensive
  }
  vars <- c(ex_vars, in_vars)
  vars_used <- unique(c(ex_vars, in_vars))
  if (isTRUE(na.rm) && length(vars_used)) {
    # ensure numeric
    bad <- vars_used[!vapply(source_sf[vars_used], is.numeric, TRUE)]
    if (length(bad)) {
      stop(
        "Non-numeric variable(s) in 'source_sf': ",
        paste(bad, collapse = ", "),
        ". Only numeric columns can be interpolated."
      )
    }
    keep_rows <- rowSums(is.na(source_sf[vars_used])) == 0L
    source_sf <- source_sf[keep_rows, , drop = FALSE]
  }
  # column presence (unchanged)
  if (!sidQN %in% names(source_sf)) {
    # column presence (unchanged)
    if (!sidQN %in% names(source_sf)) {
      stop(glue::glue("sid '{sidQN}' not found in source_sf"))
    }
    if (!tidQN %in% names(target_sf)) {
      stop(glue::glue("tid '{tidQN}' not found in target_sf"))
    }
    stop("Some 'extensive'/'intensive' vars absent from source_sf.")
  }

  # ---- CRS resolve (unchanged) ----
  source_crs <- .ducksf_resolve_crs(source_crs %||% sf::st_crs(source_sf))
  target_crs <- .ducksf_resolve_crs(target_crs %||% sf::st_crs(target_sf))
  join_crs <- .ducksf_resolve_crs(join_crs %||% target_crs)
  if (is.null(source_crs) || is.null(target_crs) || is.null(join_crs)) {
    stop(
      "Failed to resolve CRS; pass EPSG or WKT/JSON. Use projected join_crs."
    )
  }

  src_lit <- .ducksf_crs_literal(source_crs)
  trg_lit <- .ducksf_crs_literal(target_crs)
  join_lit <- .ducksf_crs_literal(join_crs)
  same_src_join <- .ducksf_crs_equal(source_crs, join_crs)
  same_trg_join <- .ducksf_crs_equal(target_crs, join_crs)

  # ---- DuckDB ----
  con <- DBI::dbConnect(duckdb::duckdb(":memory:"))
  on.exit(
    {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    },
    add = TRUE
  )
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  DBI::dbExecute(con, "CALL register_geoarrow_extensions();")
  if (!is.null(duckdb_threads)) {
    try(
      DBI::dbExecute(
        con,
        glue::glue(
          "SET threads={duckdb_threads};"
        )
      ),
      silent = TRUE
    )
  }
  # DBI::dbGetQuery(con, "SELECT current_setting('threads');") # check if set

  # ---- register data with duckdb, zero-copy ----
  .ducksf_register_sf(con, source_sf, "source_tbl_raw", overwrite = TRUE)
  .ducksf_register_sf(con, target_sf, "target_tbl_raw", overwrite = TRUE)

  # Geometry column names used below (keep the rest of the SQL unchanged)
  src_geom_name <- "geometry"
  trg_geom_name <- "geometry"

  # Normalize ids + (optional) single-pass transform to join_crs
  # SOURCE -> source_tbl (id + geom + needed vars)
  DBI::dbExecute(
    con,
    glue::glue(
      "
      CREATE OR REPLACE VIEW source_tbl AS
      SELECT
        {DBI::dbQuoteIdentifier(con, sidQN)} AS sid,
        {DBI::dbQuoteIdentifier(con, src_geom_name)} AS geom
        {if (length(vars)) paste0(', ', paste(DBI::dbQuoteIdentifier(con, vars), collapse = ', ')) else ''}
      FROM source_tbl_raw;
    "
    )
  )

  if (!isTRUE(same_src_join)) {
    DBI::dbExecute(
      con,
      glue::glue(
        "
        CREATE OR REPLACE VIEW source_proj AS
        SELECT sid, ST_Transform(geom, {src_lit}, {join_lit}) AS geom
               {if (length(vars)) paste0(', ', paste(DBI::dbQuoteIdentifier(con, vars), collapse = ', ')) else ''}
        FROM source_tbl;
      "
      )
    )
  } else {
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW source_proj AS SELECT * FROM source_tbl;"
    )
  }

  # TARGET -> target_tbl (id + geom)
  DBI::dbExecute(
    con,
    glue::glue(
      "
      CREATE OR REPLACE VIEW target_tbl AS
      SELECT
        {DBI::dbQuoteIdentifier(con, tidQN)} AS tid,
        {DBI::dbQuoteIdentifier(con, trg_geom_name)} AS geom
      FROM target_tbl_raw;
    "
    )
  )

  if (!isTRUE(same_trg_join)) {
    DBI::dbExecute(
      con,
      glue::glue(
        "
        CREATE OR REPLACE VIEW target_proj AS
        SELECT tid, ST_Transform(geom, {trg_lit}, {join_lit}) AS geom
        FROM target_tbl;
      "
      )
    )
  } else {
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW target_proj AS SELECT * FROM target_tbl;"
    )
  }

  # ---- Overlap AREAS ONLY, compute intersection once; keep table skinny ----
  DBI::dbExecute(
    con,
    "
      CREATE OR REPLACE VIEW overlap AS
      SELECT s.sid, t.tid,
             ST_Area(ST_Intersection(t.geom, s.geom)) AS overlap_area
      FROM target_proj t
      JOIN source_proj s
        ON ST_Intersects(t.geom, s.geom);
     "
  )

  # ---- Denominators (unchanged logic) ----
  if (type %in% c("extensive", "mixed")) {
    if (weight == "sum") {
      DBI::dbExecute(
        con,
        "
        CREATE OR REPLACE VIEW total_by_sid AS
        SELECT sid, SUM(overlap_area) AS total_area_sid
        FROM overlap GROUP BY sid;
        "
      )
    } else {
      DBI::dbExecute(
        con,
        "
        CREATE OR REPLACE VIEW total_by_sid AS
        SELECT sid, ST_Area(geom) AS total_area_sid
        FROM source_proj;
        "
      )
    }
  }
  if (type %in% c("intensive", "mixed")) {
    DBI::dbExecute(
      con,
      "
      CREATE OR REPLACE VIEW total_by_tid AS
      SELECT tid, SUM(overlap_area) AS total_area_tid
      FROM overlap GROUP BY tid;
      "
    )
  }

  # ---- Single-pass aggregation for all variables (replaces per-var loops) ----
  # Build SELECT expressions
  ex_exprs <- character(0)
  if (length(ex_vars)) {
    ex_exprs <- paste0(
      "SUM( (src.",
      DBI::dbQuoteIdentifier(con, ex_vars),
      ") * o.overlap_area / NULLIF(tbs.total_area_sid, 0) ) AS ",
      DBI::dbQuoteIdentifier(con, ex_vars)
    )
  }

  in_exprs <- character(0)
  if (length(in_vars)) {
    in_exprs <- paste0(
      "SUM( (src.",
      DBI::dbQuoteIdentifier(con, in_vars),
      ") * o.overlap_area / NULLIF(tbt.total_area_tid, 0) ) AS ",
      DBI::dbQuoteIdentifier(con, in_vars)
    )
  }

  select_expr <- paste(c(ex_exprs, in_exprs), collapse = ",\n      ")

  DBI::dbExecute(
    con,
    glue::glue(
      "
    CREATE OR REPLACE VIEW interpolated_all AS
    SELECT
      o.tid
      { if (nzchar(select_expr)) paste0(',\n      ', select_expr) else '' }
    FROM overlap o
    { if (length(vars)    > 0) 'JOIN source_proj src USING (sid)' else '' }
    { if (length(ex_vars) > 0) 'LEFT JOIN total_by_sid tbs USING (sid)' else '' }
    { if (length(in_vars) > 0) 'LEFT JOIN total_by_tid tbt USING (tid)' else '' }
    GROUP BY o.tid;
    "
    )
  )

  # ---- sf-ready view in original target CRS (unchanged) ----
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE VIEW interpolated_sf AS
    SELECT t.tid, t.geom AS geometry, ia.*
    FROM target_tbl t
    LEFT JOIN interpolated_all ia USING (tid);
    "
  )

  # ---- Export (unchanged) ----
  if (output == "sf") {
    x <- dplyr::tbl(con, "interpolated_sf") |> arrow::to_arrow()
    out <- sf::st_as_sf(x, crs = target_crs)
    out <- dplyr::left_join(
      target_sf,
      dplyr::as_tibble(out),
      by = dplyr::join_by(!!tidQN == "tid")
    )
    if (nameConflict) {
      out <- dplyr::rename(out, !!rlang::set_names(rlang::sym(tidQN), tidOrig))
    }
  } else {
    x <- dplyr::tbl(con, "interpolated_all") |> arrow::to_arrow()
    out <- dplyr::as_tibble(x)
    tgt_id <- dplyr::select(
      sf::st_drop_geometry(target_sf),
      !!rlang::sym(tidQN)
    )
    out <- dplyr::left_join(tgt_id, out, by = dplyr::join_by(!!tidQN == "tid"))
    if (nameConflict) {
      out <- dplyr::rename(out, !!rlang::set_names(rlang::sym(tidQN), tidOrig))
    }
  }

  # ---- keep_NA: drop targets with no contributions in requested vars ----
  if (!isTRUE(keep_NA) && length(vars_used)) {
    if (output == "sf") {
      df <- sf::st_drop_geometry(out)
      keep_t <- rowSums(!is.na(df[, vars_used, drop = FALSE])) > 0
      out <- out[keep_t, , drop = FALSE]
    } else {
      keep_t <- rowSums(!is.na(out[, vars_used, drop = FALSE])) > 0
      out <- out[keep_t, , drop = FALSE]
    }
  }

  out
}
