#' Establish database connection
#'
#' @description
#' Compatibility wrapper for [connect_creel_db()]. Retained for backwards
#' compatibility with scripts written prior to v0.2.0.
#'
#' The `conn_type` and `dsn` arguments are no longer supported. ODBC
#' connections have been removed in favor of `RPostgres` + `keyring`.
#' Calls passing `conn_type` will receive a warning and the argument will
#' be ignored. New code should use [connect_creel_db()] directly.
#'
#' @param ... Arguments passed to [connect_creel_db()]. `conn_type` and `dsn`
#'   are silently dropped with a warning if supplied.
#'
#' @return A `DBI` connection object. See [connect_creel_db()] for details.
#'
#' @seealso [connect_creel_db()]
#' @keywords internal
#' @export
establish_db_con <- function(...) {
  dots <- list(...)
  if ("conn_type" %in% names(dots)) {
    cli::cli_warn(c(
      "{.arg conn_type} is no longer supported and will be ignored.",
      "i" = "ODBC connections have been removed. Use {.fn connect_creel_db} directly.",
      "i" = "Remove {.arg conn_type} from your call to silence this warning."
    ))
    dots[["conn_type"]] <- NULL
  }
  if ("dsn" %in% names(dots)) {
    dots[["dsn"]] <- NULL
  }
  do.call(connect_creel_db, dots)
}
