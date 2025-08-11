#' Intersect geometries (DuckDB-backed, sf-compatible)
#' @export
dst_intersection <- function(
  x,
  y,
  duckdb_threads = getOption("ducksf.intersection.threads", NULL),
  compute_geometry = TRUE, # FALSE → just (xid,yid)
  join = c("intersects", "dwithin0") # "intersects" = sf-exact, "dwithin0" = fast path
) {
  join <- match.arg(join)

  # Unary/n-ary case: defer to sf for exact semantics (origins, n.overlaps, etc.)
  if (missing(y)) {
    return(sf::st_intersection(x))
  }

  # sfg → sfc
  x_is_sfg <- inherits(x, "sfg")
  if (x_is_sfg) {
    x <- sf::st_sfc(x, crs = sf::st_crs(x))
  }
  if (inherits(y, "sfg")) {
    y <- sf::st_sfc(y, crs = sf::st_crs(y))
  }

  # CRS must match (sf semantics)
  if (!isTRUE(sf::st_crs(x) == sf::st_crs(y))) {
    stop("st_crs(x) and st_crs(y) must be identical; transform first.")
  }
  out_crs <- sf::st_crs(x)

  # Wrap as sf for registration/attrs
  x_sf <- if (inherits(x, "sf")) x else sf::st_sf(geometry = x)
  y_sf <- if (inherits(y, "sf")) y else sf::st_sf(geometry = y)

  con <- ducksf_duck_connect(threads = duckdb_threads)
  on.exit(ducksf_duck_disconnect(con), add = TRUE)
  .ducksf_register_sf(con, x_sf, "x_tbl", overwrite = TRUE)
  .ducksf_register_sf(con, y_sf, "y_tbl", overwrite = TRUE)

  on_pred <- if (join == "dwithin0") {
    "ST_DWithin(x.geometry, y.geometry, 0)"
  } else {
    "ST_Intersects(x.geometry, y.geometry)"
  }
  select_geom <- if (isTRUE(compute_geometry)) {
    "ST_Intersection(x.geometry, y.geometry) AS geometry"
  } else {
    "NULL::GEOMETRY AS geometry"
  }

  DBI::dbExecute(
    con,
    glue::glue(
      "
      CREATE OR REPLACE VIEW _ducksf_pairs AS
      WITH
        x AS (SELECT ROW_NUMBER() OVER () AS xid, geometry FROM x_tbl),
        y AS (SELECT ROW_NUMBER() OVER () AS yid, geometry FROM y_tbl)
      SELECT x.xid, y.yid, {select_geom}
      FROM x
      JOIN y
        ON {on_pred}
      ORDER BY x.xid, y.yid;  -- deterministic for tests
    "
    )
  )

  res <- dplyr::tbl(con, "_ducksf_pairs") |>
    arrow::to_arrow() |>
    as.data.frame()
  xid <- as.integer(res$xid)
  yid <- as.integer(res$yid)

  if (inherits(x, "sf")) {
    df_x <- sf::st_drop_geometry(x_sf)[xid, , drop = FALSE]
    if (inherits(y, "sf")) {
      df_y <- sf::st_drop_geometry(y_sf)[yid, , drop = FALSE]
      df <- data.frame(df_x, df_y, check.names = TRUE)
    } else {
      df <- df_x
    }
    geom <- if (isTRUE(compute_geometry)) {
      sf::st_as_sfc(res$geometry, crs = out_crs)
    } else {
      sf::st_sfc(rep(sf::st_geometrycollection(), nrow(res)), crs = out_crs)
    }
    sf_col <- attr(x_sf, "sf_column")
    df[[sf_col]] <- geom
    return(sf::st_sf(df, sf_column_name = sf_col))
  }

  geom <- if (isTRUE(compute_geometry)) {
    sf::st_as_sfc(res$geometry, crs = out_crs)
  } else {
    sf::st_sfc(rep(sf::st_geometrycollection(), nrow(res)), crs = out_crs)
  }
  idx <- cbind(xid, yid)
  dimnames(idx) <- NULL
  attr(geom, "idx") <- idx

  if (x_is_sfg) {
    if (!length(geom)) {
      return(sf::st_geometrycollection())
    }
    return(geom[[1]])
  }
  geom
}
