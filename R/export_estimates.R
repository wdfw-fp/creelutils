#' Export creel model estimates
#'
#' Primary control function of the export, transform, and load (ETL) process. Takes standardized model outputs and user-input parameters to export the model estimates appropriately.
#'
#' @family ETL
#' @param resolved_params User-input parameters defined during the model estimation process.
#' @param est_catch_groups Output of `resolve_catch_groups()`.
#' @param analysis_lut Lookup table created during the model estimation process which stores a session-specific analysis_id key and metadata about the analysis.
#' @param creel_estimates List object containing model estimates in a standardized format. Typically passed from 'transform_estimates' function.
#' @param conn Connection to the creel database.
#'
#' @return Invisibly, NULL. Called for side effects.
#' @export
#'
export_estimates <- function(resolved_params, est_catch_groups, analysis_lut, creel_estimates, conn) {

  # Normalize and validate params
  export <- tolower(resolved_params$export)
  export_tables <- tolower(resolved_params$export_tables)
  export_types <- c("database", "local", "no")
  export_tables_types <- c("both", "total", "stratum")

  if (!export %in% export_types) {
    cli::cli_abort("Invalid {.field params$export} entered. Valid export types: {export_types}")
  }
  # Export to database ---------------------------------------------------------------------------
  if(export == "database") {

    # Validate connection; lazily open if NULL, fail if supplied `conn` is invalid
    conn <- .resolve_conn(conn)

    if (!export_tables %in% export_tables_types) {
      cli::cli_abort("Invalid {.field params$export_tables} entered. Valid export table types: {export_tables_types}")
    }

    # Add additional metadata fields to analysis lut
    analysis_lut <- finalize_analysis_lut(analysis_lut = analysis_lut, resolved_params = resolved_params, conn = conn)

    # Query database for lookup table UUIDs and drop human-readable fields
    # fishery_name for fishery_id, est_cg for model_catch_group_id, etc.
    creel_estimates_db <- prep_export(conn = conn, creel_estimates, catch_group_lut = est_catch_groups)

    # Record row counts to validate upload against
    intended <- c(
      lut = nrow(analysis_lut),
      total = nrow(creel_estimates_db$total),
      stratum = nrow(creel_estimates_db$stratum))

    ## Write estimates --------------------------------------------------------------------------
    existing_id_check <- fetch_db_table(
      conn = conn, schema =  "creel", table = "model_analysis_lut",
      filter = glue::glue("analysis_id == '{analysis_lut$analysis_id}'")
    )

    if (nrow(existing_id_check) > 0) {
      cli::cli_abort("Duplicate {.val {analysis_lut$analysis_id}} detected. Database upload aborted.")
    } else {
      if (export_tables == "total") { # write lut and total

        DBI::dbWithTransaction(conn = conn, {
          write_lut(conn = conn, analysis_lut)
          write_total(conn = conn, creel_estimates_db$total)
        })
      } else if (export_tables == "stratum") { # write lut and stratum

        DBI::dbWithTransaction(conn = conn, {
          write_lut(conn = conn, analysis_lut)
          write_stratum(conn = conn, creel_estimates_db$stratum)
        })
      } else if (export_tables == "both") {
        # Write lut, total, and stratum
        DBI::dbWithTransaction(conn = conn, {
          n <- c(
            lut = write_lut(conn = conn, analysis_lut),
            total = write_total(conn = conn, creel_estimates_db$total),
            stratum = write_stratum(conn = conn, creel_estimates_db$stratum)
          )
          # Verify row counts on each table before exiting transaction
          if (!all(n == intended)) {
            failed <- names(intended)[n != intended]
            cli::cli_abort(c(
              "Row count mismatch; transaction rolled back.",
              "x" = "Mismatched table{?s}: {.field {failed}}.",
              "i" = "Intended: {intended[failed]}. Failed attempt: {n[failed]}."
            ))
          }
        })
      }
    # Successful write transaction notification
    cli::cli_alert_success("Export successful. Creel estimates and analysis metadata.")
    cli::cli_alert_info("analysis_id: {.strong {analysis_lut$analysis_id}}")
    }
  # Export locally ------------------------------------------------------------------------------
  } else if (export == "local") {
    # #process for exporting ETL output tables locally for inspection prior to uploading to database
    #
    # # Add additional metadata fields to analysis lut
    # # Project name and fishery name not changed to ids
    # analysis_lut <- finalize_analysis_lut(analysis_lut, resolved_params, conn = NULL)
    #
    # #project- and fishery-specific folder from CreelEstimates
    # #could be more flexible and make folders where needed? for case of recreation of script on computer that did run analysis
    # write_directory <- paste0(getwd(), "/fishery_analyses/", resolved_params$project_name, "/", resolved_params$fishery_name,"/")
    #
    # #write csv files to local working directory
    # readr::write_csv(analysis_lut, file = paste0(write_directory,"analysis_lut.csv"))
    # readr::write_csv(creel_estimates$stratum, file = paste0(write_directory,"model_estimates_stratum.csv"))
    # readr::write_csv(creel_estimates$total, file = paste0(write_directory, "model_estimates_total.csv"))
    #
    # cat("\n\n")
    # cat("Standardized model estimate tables and analysis_lut saved to fishery folder on local computer.")
    cli::cli_inform("Local export option internal to {.fun export_estimates} is currently disabled
                    in favor of local export option in analysis script.")

  } else {
    cli::cli_inform("{.field creel_estimates} not exported. Set {.field params$export} to {.q local} or {.q database}.")
  }
  return(invisible(NULL))
}
