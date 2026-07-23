#' Confirm an analysis_id exists in the database
#'
#' @param conn An open DBI connection to the creel database.
#' @param analysis_id character; session-specific analysis_id created by `generate_analysis_lut()`
#'
#' @return Invisibly, TRUE if the analysis_id was found, otherwise FALSE.
#' @export
confirm_db_upload <- function(conn, analysis_id) {

  id <- fetch_db_table(
    conn = conn, schema =  "creel", table = "model_analysis_lut",
    filter = glue::glue("analysis_id == '{analysis_id}'")
  )

  if (nrow(id) > 0) {
    cli::cli_alert_success("Confirmed {.field analysis_id} in {.field model_analysis_lut}.")
  } else {
    cli::cli_alert_danger("Unable to confirm that {.val {analysis_id}} is in {.field model_analysis_lut}")
    cli::cli_warn("Manual review required. Tables to inspect:
                  {.field model_analysis_lut}, {.field model_estimates_total}, and {.field model_estimates_stratum}")
  }
  return(invisible(nrow(id) > 0))
}
