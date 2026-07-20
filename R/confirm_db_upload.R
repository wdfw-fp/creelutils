#' Confirm database upload
#'
#'Confirms upload of model estimates by querying model_analysis_lut for session analysis_id.
#'
#' @param conn Connection to WDFW PostgreSQL database made with DBI-compliant RPostgres package. 'con' created by establish_db_con.R function.
#' @param analysis_lut Data frame containing session-specific analysis_id and associated metadata created by generate_analysis_lut.R
#'
#' @return nothing returned.
#' @export
#'
confirm_db_upload <- function(conn, analysis_lut) {

  verification_table <- fetch_db_table(conn = conn, schema =  "creel", table = "model_analysis_lut") |>
    dplyr::select("analysis_id")

  if (analysis_lut$analysis_id %in% verification_table$analysis_id) {

    cat("Confirmed that 'analysis_id' exists in the 'model_analysis_lut' table.")

  } else {
    #what to do if analysis_id is not in analysis_lut (partial/failed export)
    message("\nUnable to confirm upload by checking database for session analysis_id.")
    message(paste("\nWriting 'FAILED_UPLOAD_LOG_analysis_lut.csv' to CreelEstimates folder so that analysis_id for partial data upload can be investigated."))
    readr::write_csv(analysis_lut, file = paste0("FAILED_UPLOAD_LOG_","analysis_lut.csv"), append = TRUE)
  }
}
