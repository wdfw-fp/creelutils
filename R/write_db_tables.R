#' Write look-up table
#'
#' @family ETL
#' @param conn a valid `DBI` connection. @seealso [establish_db_con()]
#' @param analysis_lut lookup table created during the model estimation process which stores a session-specific analysis_id key and metadata about the analysis.
#' @param max_retries maximum number of times to try to write; numeric, defaults to 5.
#'
#' @export
#'
write_lut <- function(conn, analysis_lut, max_retries = 5) {

  #define function to check for NOT NULL constraints in analysis_lut
  validate_lut <- function(data) {
    required_columns <- c(
      "analysis_id", "project_id", "fishery_id", "data_grade", "model_run_type",
      "git_sha", "analysis_folder_name", "params_json", "r_session_json")
    #absent columns must fail, not silently pass (any(is.na(NULL)) == FALSE)
    absent_columns <- setdiff(required_columns, names(data))
    if (length(absent_columns) > 0) {
      stop("The following required columns are absent: ", paste(absent_columns, collapse = ", "))
    }
    missing_values <- sapply(required_columns, function(col) any(is.na(data[[col]])))
    if (any(missing_values)) {
      stop("The following columns contain NA values: ", paste(names(missing_values)[missing_values], collapse = ", "))
    }
    return(data)
  }

  #ensure data adheres to NOT NULL constraints
  analysis_lut <- validate_lut(analysis_lut)

  attempt <- 1
  success <- FALSE

  #attempt to write table, retrying if any NOT NULL constraints are violated
  while (attempt <= max_retries && !success) {
    tryCatch({
      DBI::dbWriteTable(
        conn = conn,
        name = DBI::Id(schema = "creel", table = "model_analysis_lut"),
        value = analysis_lut,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE
      )

      success <- TRUE
      message("Data written successfully on attempt ", attempt)

    }, error = function(e) {
      message("Attempt ", attempt, " failed: ", e$message)
      attempt <<- attempt + 1

      if (attempt > max_retries) {
        stop("Failed to write data after ", max_retries, " attempts.")
      }
    })
  }
}

#' Write Total
#'
#' @family ETL
#' @param conn a valid DBI connection. @seealso [establish_db_con()]
#' @param creel_estimates_db a list object containing the standardized model outputs that have been processed by `prep_export()` to join certain fields with database lookup tables prior to exportation.
#'
#' @export
#'
write_total <- function(conn, creel_estimates_db) {
  DBI::dbWriteTable(
    conn = conn,
    name = DBI::Id(schema = "creel", table = "model_estimates_total"),
    value = creel_estimates_db$total,
    row.names = FALSE,
    overwrite = FALSE,
    append = TRUE)
}

#' Write stratum
#'
#' @family ETL
#' @param conn a valid DBI connection. @seealso [establish_db_con()]
#' @param creel_estimates_db a list object containing the standardized model outputs that have been processed by `prep_export()` to join certain fields with database lookup tables prior to exportation.
#'
#' @export
#'
write_stratum <- function(conn, creel_estimates_db) {
  DBI::dbWriteTable(
    conn = conn,
    name = DBI::Id(schema = "creel", table = "model_estimates_stratum"),
    value = creel_estimates_db$stratum,
    row.names = FALSE,
    overwrite = FALSE,
    append = TRUE)
}
