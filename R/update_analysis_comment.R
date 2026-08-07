#' Update the comment on an existing analysis record
#'
#' @param conn An open DBI connection to the creel database.
#' @param analysis_id UUID of the analysis row to update.
#' @param comment New comment text.
#'
#' @return Invisibly, the number of rows updated (1 on success).
#' @family ETL
#' @export
#'
#' @examples
#' \dontrun{
#' update_analysis_comment(
#'   con,
#'   analysis_id = "d609a0e4-653c-4225-b6e3-24c9b236f882",
#'   comment = "Re-upload after filtering outlier interview in section 3 on the 12th"
#' )
#' }
update_analysis_comment <- function(conn, analysis_id, comment) {

  # Validate connection; lazily open if NULL, fail if supplied `conn` is invalid
  conn <- .resolve_conn(conn)

  # Confirm the target exists and capture existing comment
  current <- fetch_db_table(
    conn,
    schema = "creel",
    table  = "model_analysis_lut",
    filter = glue::glue("analysis_id == '{analysis_id}'")
  )
  if (nrow(current) == 0L) {
    cli::cli_abort("No analysis found with {.field analysis_id} {.val {analysis_id}}.")
  }

  # Send update query
  n <- DBI::dbExecute(
    conn,
    "UPDATE creel.model_analysis_lut
        SET comment_txt = $1
      WHERE analysis_id = $2",
    params = list(comment, analysis_id)
  )

  old <- current$comment_txt[[1]]
  if (is.na(old)) {
    cli::cli_alert_success("Set comment on {.val {analysis_id}}.")
  } else {
    cli::cli_alert_success("Replaced comment on {.val {analysis_id}} (was: {.val {old}}).")
  }

  invisible(n)
}
