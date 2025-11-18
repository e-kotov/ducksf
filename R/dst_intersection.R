#' Intersect geometries (DuckDB-backed, sf-compatible)
#' @export
dst_intersection <- function(
  x,
  y,
  duckdb_threads = getOption("ducksf.intersection.threads", NULL),
  join = c("intersects", "dwithin0"), # join predicate used to trigger SPATIAL_JOIN
  compute_geometry = TRUE, # FALSE: only pairs (with empty geoms)
  deterministic_order = TRUE, # ORDER BY xid, yid in SQL
  keep_pair_ids = FALSE, # keep internal pair ids (__xid__, __yid__) in sf output (handy for tests)
  sf_fallback_threshold = 3e4 # quick sfc×sfc fallback to sf when tiny
) {
  join <- match.arg(join)

  # Unary/n-ary: keep sf semantics (origins/n.overlaps etc.)
  if (missing(y)) {
    return(sf::st_intersection(x))
  }

  # sfg → sfc wrappers
  x_is_sfg <- inherits(x, "sfg")
  if (x_is_sfg) {
    x <- sf::st_sfc(x, crs = sf::st_crs(x))
  }
  if (inherits(y, "sfg")) {
    y <- sf::st_sfc(y, crs = sf::st_crs(y))
  }

  # CRS equality exactly (sf semantics)
  if (!isTRUE(sf::st_crs(x) == sf::st_crs(y))) {
    stop("st_crs(x) and st_crs(y) must be identical; transform first.")
  }
  out_crs <- sf::st_crs(x)

  # small sfc×sfc jobs: sf is usually faster than setting up DuckDB/Arrow
  if (!inherits(x, "sf") && !inherits(y, "sf")) {
    est_pairs <- length(x) * length(y)
    if (est_pairs <= sf_fallback_threshold) {
      res <- sf::st_intersection(x, y)
      return(
        if (x_is_sfg) {
          if (length(res)) res[[1]] else sf::st_geometrycollection()
        } else {
          res
        }
      )
    }
  }

  # Wrap as sf to register
  x_sf <- if (inherits(x, "sf")) x else sf::st_sf(geometry = x)
  y_sf <- if (inherits(y, "sf")) y else sf::st_sf(geometry = y)

  con <- ducksf_duck_connect(threads = duckdb_threads)
  on.exit(ducksf_duck_disconnect(con), add = TRUE)

  .ducksf_register_sf(con, x_sf, "x_tbl", overwrite = TRUE)
  .ducksf_register_sf(con, y_sf, "y_tbl", overwrite = TRUE)

  on_pred <- if (join == "dwithin0") {
    # Often faster on DuckDB ≥ 1.3 due to native implementation
    "ST_DWithin(x.geometry, y.geometry, 0)"
  } else {
    "ST_Intersects(x.geometry, y.geometry)"
  }

  geom_sql <- if (isTRUE(compute_geometry)) {
    "ST_AsWKB(ST_Intersection(x.geometry, y.geometry)) AS wkb"
  } else {
    "NULL::BLOB AS wkb"
  }

  order_sql <- if (isTRUE(deterministic_order)) "ORDER BY xid, yid" else ""

  # IMPORTANT: filter empties (matches sf::st_intersection(sfc, sfc) behavior)
  sql <- glue::glue(
    "
    WITH
      x AS (SELECT ROW_NUMBER() OVER () AS xid, geometry FROM x_tbl),
      y AS (SELECT ROW_NUMBER() OVER () AS yid, geometry FROM y_tbl),
      j AS (
        SELECT x.xid, y.yid, {geom_sql}
        FROM x
        JOIN y
          ON {on_pred}
      )
    SELECT *
    FROM j
    WHERE { if (compute_geometry) 'NOT ST_IsEmpty(ST_GeomFromWKB(wkb))' else 'TRUE' }
    {order_sql};
    "
  )

  res <- DBI::dbGetQuery(con, sql)
  xid <- as.integer(res$xid)
  yid <- as.integer(res$yid)

  # ---------------- sfc output ----------------
  if (!inherits(x, "sf")) {
    geom <- if (isTRUE(compute_geometry)) {
      sf::st_as_sfc(wk::wkb(res$wkb), crs = out_crs)
    } else {
      sf::st_sfc(rep(sf::st_geometrycollection(), nrow(res)), crs = out_crs)
    }
    idx <- cbind(xid, yid)
    dimnames(idx) <- NULL
    attr(geom, "idx") <- idx

    if (x_is_sfg) {
      return(if (length(geom)) geom[[1]] else sf::st_geometrycollection())
    }
    return(geom)
  }

  # ---------------- sf output ----------------
  df_x <- sf::st_drop_geometry(x_sf)[xid, , drop = FALSE]
  df_y <- if (inherits(y, "sf")) {
    sf::st_drop_geometry(y_sf)[yid, , drop = FALSE]
  } else {
    NULL
  }
  df <- if (!is.null(df_y)) data.frame(df_x, df_y, check.names = TRUE) else df_x

  if (isTRUE(keep_pair_ids)) {
    # expose ids for deterministic ordering in tests
    df$.__xid__ <- xid
    df$.__yid__ <- yid
  }

  geom <- if (isTRUE(compute_geometry)) {
    sf::st_as_sfc(wk::wkb(res$wkb), crs = out_crs)
  } else {
    sf::st_sfc(rep(sf::st_geometrycollection(), nrow(res)), crs = out_crs)
  }

  sf_col <- attr(x_sf, "sf_column")
  df[[sf_col]] <- geom
  sf::st_sf(df, sf_column_name = sf_col)
}
