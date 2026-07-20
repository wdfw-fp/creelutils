#' Export creel model estimates
#'
#' Primary control function of the export, transform, and load (ETL) process. Takes standardized model outputs and user-input parameters to export the model estimates appropriately.
#'
#' @family ETL
#' @param resolved_params User-input parameters defined during the model estimation process.
#' @param est_catch_groups Output of `resolve_catch_groups()`.
#' @param analysis_lut Lookup table created during the model estimation process which stores a session-specific analysis_id key and metadata about the analysis.
#' @param creel_estimates List object containing model estimates in a standardized format. Typically passed from 'transform_estimates' function.
#' @param conn Database connection object. If NULL (default), a new connection will be established.
#'
#' @return ??
#' @export
#'
export_estimates <- function(resolved_params, est_catch_groups, analysis_lut, creel_estimates, conn = NULL) {

  # Connect to database and conditionally export
  if(resolved_params$export == tolower("database")) {

    # Connect to database if connection not supplied in argument
    if (is.null(conn)) {
    con <- creelutils::connect_creel_db()
    } else {
      con <- conn # Use connection provided
    }

    # Add additional metadata fields to analysis lut
    analysis_lut <- finalize_analysis_lut(analysis_lut = analysis_lut, resolved_params = resolved_params, conn = con)

    #query database for UUIDs and reformat
    creel_estimates_db <- prep_export(conn = con, creel_estimates, catch_group_lut = est_catch_groups)

    ### write estimates to database ####

    #model_analysis_lut
    #determine if session analysis_id already exists in database model_analysis_lut table
    cat("\nVerifying that session 'analysis_id' does not exist in database before upload.")
    analysis_id_check <- fetch_db_table(con, "creel", "model_analysis_lut") |> dplyr::select("analysis_id")

    if (analysis_lut$analysis_id %in% analysis_id_check$analysis_id) {
      cat("\n")
      stop("\nAnalysis uuid already exists in the creel database. Review before proceeding.")
    } else { #analysis_id not already in database

        #evaluate export_tables parameter
        if (resolved_params$export_tables == "total") {

          #write lut and total
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut(conn = con, analysis_lut)

          cat(paste0("Writing to model_estimates_total table...  ","\u2713", "\n"))
          write_total(conn = con, creel_estimates_db)

        } else if (resolved_params$export_tables == "stratum") {

          #write lut and stratum
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut(conn = con, analysis_lut)

          cat(paste0("Writing to model_estimates_stratum table...  ","\u2713", "\n"))
          write_stratum(conn = con, creel_estimates_db)

        } else if (resolved_params$export_tables == "both") {

          #write lut, total, and stratum
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut(conn = con, analysis_lut)

          cat(paste0("Writing to model_estimates_total table...  ","\u2713", "\n"))
          write_total(conn = con, creel_estimates_db)

          cat(paste0("Writing to model_estimates_stratum table...  ","\u2713", "\n"))
          write_stratum(conn = con, creel_estimates_db)

        } else {
          cat("\nParameter export_tables must be either 'total', 'stratum', or 'both'.")
        }
    }

    #verify that estimates have been written as expected
    cat("\nUploading complete. Verifying session 'analysis_id' in database analysis look up table.")
    confirm_db_upload(conn = con, analysis_lut)

    #local export option
  } else if (resolved_params$export == tolower("local")) {
    #process for exporting ETL output tables locally for inspection prior to uploading to database

    # Add additional metadata fields to analysis lut
    # Project name and fishery name not changed to ids
    analysis_lut <- finalize_analysis_lut(analysis_lut, resolved_params, conn = NULL)

    #project- and fishery-specific folder from CreelEstimates
    #could be more flexible and make folders where needed? for case of recreation of script on computer that did run analysis
    write_directory <- paste0(getwd(), "/fishery_analyses/", resolved_params$project_name, "/", resolved_params$fishery_name,"/")

    #write csv files to local working directory
    readr::write_csv(analysis_lut, file = paste0(write_directory,"analysis_lut.csv"))
    readr::write_csv(creel_estimates$stratum, file = paste0(write_directory,"model_estimates_stratum.csv"))
    readr::write_csv(creel_estimates$total, file = paste0(write_directory, "model_estimates_total.csv"))

    cat("\n\n")
    cat("Standardized model estimate tables and analysis_lut saved to fishery folder on local computer.")

    #do not write out estimates
  } else if (resolved_params$export == tolower("No")) {
    #send message to user that no ETL actions were taken
    cat("\n\n")
    cat("Catch and effort estimates not exported.")
    cat("\nStandardized model estimates can be viewed in output list object 'creel_estimates'.")

  } else {
    #send message to user with correct export parameter options
    cat("Export parameter must be either 'no', 'local', or 'database'.")
  }
}
