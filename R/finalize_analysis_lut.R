#' Finalize the analysis look-up table for export
#'
#' Completes the session `analysis_lut` created by `generate_analysis_lut()`
#' immediately prior to export. Adds ETL-time metadata (r_session_json,
#' data_grade), and, when a database connection is supplied, resolves
#' project/fishery names to database UUIDs and validates the final column
#' set against the model_analysis_lut schema.
#'
#' With `con = NULL` (local export) the lut retains `project_name` and
#' `fishery_name`; id resolution is deterministic and occurs whenever the
#' session is later exported to the database.
#'
#' @family ETL
#' @param analysis_lut lookup table created during the model estimation
#'   process which stores a session-specific analysis_id key and metadata
#'   about the analysis.
#' @param resolved_params user-input parameters defined during the model estimation
#'   process.
#' @param conn a valid `DBI` connection, or NULL for local export.
#'   @seealso [connect_creel_db()]
#'
#' @return The finalized analysis_lut, ready for `write_lut()` (database)
#'   or local archiving.
#' @export
finalize_analysis_lut <- function(analysis_lut, resolved_params, conn = NULL) {

  # ETL 1.0 tripwire ------------------------------------------------------
  # analysis_name was dropped from the schema; its presence reliably
  # identifies a legacy lut (e.g. an old .rds reloaded from a pre-2.0
  # analysis session folder).
  if ("analysis_name" %in% names(analysis_lut)) {
    cli::cli_abort(c(
      "Legacy (ETL 1.0) analysis_lut detected.",
      "x" = "Column {.field analysis_name} is no longer part of the model_analysis_lut schema.",
      "i" = "Regenerate this analysis session with the updated {.fn generate_analysis_lut}."
    ))
  }

  # ETL-time metadata -----------------------------------------------------
  # r_session_json (script/regulations json remain pending; their columns
  # are omitted and default to NULL on the database side)
  analysis_lut <- json_conversion(type = "r_session", params = resolved_params, analysis_lut)

  # validate and add data grade
  if (is.null(resolved_params$data_grade)) {
    cli::cli_abort("resolved_params$data_grade is NULL; data_grade is required for model_analysis_lut.")
  }

  analysis_lut <- analysis_lut |>
    dplyr::mutate(data_grade = resolved_params$data_grade)

  if (!"comment_txt" %in% names(analysis_lut)) {
    analysis_lut <- analysis_lut |>
      dplyr::mutate(comment_txt = NA_character_)
  }

  # Columns generate_analysis_lut() must have supplied, regardless of branch
  required_local <- c(
    "analysis_id", "project_name", "fishery_name",
    "model_run_type", "git_sha", "git_tag",
    "analysis_folder_name", "params_json"
  )
  missing_local <- setdiff(required_local, names(analysis_lut))
  if (length(missing_local) > 0) {
    cli::cli_abort("analysis_lut is missing required column{?s}: {.field {missing_local}}.")
  }

  # Local branch: stop here, names retained -------------------------------
  if (is.null(conn)) {
    cat("\nanalysis_lut finalized for local export")
    return(analysis_lut)
  }

  # Database branch: resolve names to UUIDs -------------------------------
  cat("\nResolving project and fishery UUIDs for analysis_lut.")

  project_lut <- fetch_db_table(conn, schema = "creel", table = "project_lut") |>
    dplyr::select("project_name", "project_id")
  fishery_lut <- fetch_db_table(conn, schema = "creel", table = "fishery_lut") |>
    dplyr::select("fishery_name", "fishery_id")

  analysis_lut <- analysis_lut |>
    dplyr::left_join(project_lut, by = "project_name") |>
    dplyr::left_join(fishery_lut, by = "fishery_name")

  if (is.na(analysis_lut$project_id) || is.na(analysis_lut$fishery_id)) {
    cli::cli_abort(c(
      "Could not resolve database UUIDs for analysis_lut.",
      "x" = "project_id resolved: {.val {!is.na(analysis_lut$project_id)}}",
      "x" = "fishery_id resolved: {.val {!is.na(analysis_lut$fishery_id)}}",
      "i" = "Check {.val {analysis_lut$project_name}} / {.val {analysis_lut$fishery_name}} against project_lut / fishery_lut."
    ))
  }

  # Drop local-only name columns in favor of UUIDs
  analysis_lut <- analysis_lut |>
    dplyr::select(-"project_name", -"fishery_name") |>
    dplyr::relocate("analysis_id", "project_id", "fishery_id")

  # Validate against the database contract --------------------------------
  db_contract <- c(
    "analysis_id", "project_id", "fishery_id", "data_grade",
    "model_run_type", "git_sha", "git_tag", "analysis_folder_name",
    "params_json", "analysis_json", "r_session_json",
    "fishery_regulation_json", "comment_txt"
  )

  extra_cols <- setdiff(names(analysis_lut), db_contract)
  if (length(extra_cols) > 0) {
    cli::cli_abort(c(
      "analysis_lut contains column{?s} not in the model_analysis_lut schema: {.field {extra_cols}}.",
      "i" = "Remove or rename before writing to the database."
    ))
  }

  required_db <- setdiff(db_contract, c("analysis_json", "fishery_regulation_json"))
  missing_db <- setdiff(required_db, names(analysis_lut))
  if (length(missing_db) > 0) {
    cli::cli_abort(
      "analysis_lut is missing required model_analysis_lut column{?s}: {.field {missing_db}}."
    )
  }

  cat("\nanalysis_lut finalized and validated against model_analysis_lut schema.")

  return(analysis_lut)
}
