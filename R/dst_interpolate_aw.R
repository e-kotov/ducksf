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
  # ---- ids: accept bare names or strings, then canonicalize ----
  sidQN <- .ducksf_normalize_cols(rlang::enexpr(sid), "sid")
  tidQN <- .ducksf_normalize_cols(rlang::enexpr(tid), "tid")

  # required columns present?
  ducksf_check_cols_present(source_sf, sidQN, "source_sf")
  ducksf_check_cols_present(target_sf, tidQN, "target_sf")

  # handle tid/sid name conflict on target (renames target tid -> ...tid if needed)
  cf <- ducksf_handle_id_conflict(target_sf, tidQN, sidQN)
  target_sf <- cf$target_sf
  tidQN <- cf$tidQN
  nameConflict <- cf$nameConflict
  tidOrig <- cf$tidOrig

  # plan variables: figure extensive/intensive/mixed, enforce numeric, apply na.rm
  pv <- ducksf_interpolate_aw_plan_vars(
    source_sf,
    extensive,
    intensive,
    na.rm = na.rm
  )
  type <- pv$type
  ex_vars <- pv$ex_vars
  in_vars <- pv$in_vars
  vars_used <- pv$vars_used
  source_sf <- pv$source_sf
  vars <- c(ex_vars, in_vars)

  # sf-style constraint: intensive-only cannot use weight = "total"
  if (identical(type, "intensive") && identical(weight, "total")) {
    stop("Intensive requires weight='sum'.")
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
  con <- ducksf_duck_connect(threads = duckdb_threads)
  on.exit(ducksf_duck_disconnect(con), add = TRUE)

  # ---- register data with duckdb, zero-copy ----
  .ducksf_register_sf(con, source_sf, "source_tbl_raw", overwrite = TRUE)
  .ducksf_register_sf(con, target_sf, "target_tbl_raw", overwrite = TRUE)

  # Geometry column names used below (keep the rest of the SQL unchanged)
  src_geom_name <- "geometry"
  trg_geom_name <- "geometry"

  # Normalize ids + (optional) single-pass transform to join_crs
  # SOURCE -> source_tbl (id + geom + needed vars)
  ducksf_build_proj_views(
    con = con,
    sidQN = sidQN,
    tidQN = tidQN,
    src_lit = src_lit,
    trg_lit = trg_lit,
    join_lit = join_lit,
    same_src_join = same_src_join,
    same_trg_join = same_trg_join,
    vars = vars,
    source_tbl_raw = "source_tbl_raw",
    target_tbl_raw = "target_tbl_raw",
    src_geom_name = "geometry",
    trg_geom_name = "geometry"
  )

  # ---- Overlap ----
  ducksf_build_overlap(
    con,
    src_view = "source_proj",
    trg_view = "target_proj",
    out_view = "overlap",
    join_pred = "intersects",
    measure = "area",
    keep_geom = FALSE,
    materialize = FALSE,
    prefilter_bbox = FALSE
  )

  # ---- Denominators ----
  if (type %in% c("extensive", "mixed")) {
    ducksf_build_denom(
      con,
      by = "sid",
      mode = weight, # "sum" or "total"
      measure = "area",
      overlap_view = "overlap",
      src_proj_view = "source_proj",
      out_view = "total_by_sid",
      alias = "total_area_sid"
    )
  }
  if (type %in% c("intensive", "mixed")) {
    ducksf_build_denom(
      con,
      by = "tid",
      mode = "sum",
      measure = "area",
      overlap_view = "overlap",
      out_view = "total_by_tid",
      alias = "total_area_tid"
    )
  }

  # ---- Single-pass aggregation for all variables (replaces per-var loops) ----
  ducksf_compile_aggs(
    con,
    ex_vars = ex_vars,
    in_vars = in_vars,
    measure = "area",
    overlap_view = "overlap",
    source_proj_view = "source_proj",
    denom_sid_view = "total_by_sid",
    denom_tid_view = "total_by_tid",
    denom_sid_alias = "total_area_sid",
    denom_tid_alias = "total_area_tid",
    out_view = "interpolated_all",
    tid_col = "tid"
  )

  # ---- sf-ready view in original target CRS (unchanged) ----
  ducksf_build_interpolated_sf_view(
    con,
    target_tbl = "target_tbl",
    ia_view = "interpolated_all",
    out_view = "interpolated_sf"
  )

  # ---- collect output ----
  out <- ducksf_collect_output(
    con,
    output = output,
    target_sf = target_sf,
    target_crs = target_crs,
    tidQN = tidQN,
    nameConflict = nameConflict,
    tidOrig = tidOrig,
    ia_view = "interpolated_all",
    isf_view = "interpolated_sf"
  )

  # ---- keep_NA: drop targets with no contributions in requested vars ----
  out <- ducksf_apply_keep_na(
    out,
    output = output,
    vars_used = vars_used,
    keep_NA = keep_NA
  )

  out
}
