#' @keywords internal
ducksf_duck_connect <- function(
  threads = NULL,
  load_spatial = TRUE,
  load_geoarrow = TRUE,
  database = ":memory:",
  read_only = FALSE
) {
  con <- DBI::dbConnect(duckdb::duckdb(database, read_only = read_only))

  if (isTRUE(load_spatial)) {
    # INSTALL is idempotent; keep it robust across DuckDB versions
    try(DBI::dbExecute(con, "INSTALL spatial;"), silent = TRUE)
    DBI::dbExecute(con, "LOAD spatial;")
  }

  if (isTRUE(load_geoarrow)) {
    # Available in recent DuckDB builds; ignore if missing
    try(
      DBI::dbExecute(con, "CALL register_geoarrow_extensions();"),
      silent = TRUE
    )
  }

  if (!is.null(threads)) {
    threads <- as.integer(threads)
    if (!is.na(threads) && threads > 0L) {
      try(
        DBI::dbExecute(con, paste0("SET threads=", threads, ";")),
        silent = TRUE
      )
    }
  }

  con
}

#' @keywords internal
ducksf_duck_disconnect <- function(con) {
  try(
    {
      if (inherits(con, "DBIConnection") && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    },
    silent = TRUE
  )
  invisible(TRUE)
}
