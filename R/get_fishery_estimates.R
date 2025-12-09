#' Obtain and format creel estimates for sets of fishery-years
#'
#' @description Query model estimates from the creel database for specified fisheries and years.
#' Returns formatted estimate data with catch groups and metadata. Based on approach used in
#' `get_fishery_data()`.
#'
#' @family internal_data
#' @param fishery_names Character string of fishery name or vector of character strings.
#'   If `years` is provided, `fishery_names` is combined with `year` to create fishery names.
#'   If `years` is not provided, uses this character string or vector of character strings as
#'   the exact fishery names. Try `search_fishery_name()` to see available options.
#' @param years Integer or vector of integers identifying years of data to pull.
#'   Optional argument, defaults to NULL.
#' @param max_upload_date Logical. If TRUE (default), filters to only the most recent upload
#'   date for each fishery_name and catch_group combination. If FALSE, returns all estimates
#'   for each fishery.
#' @param con A valid connection to the WDFW PostgreSQL database. If NULL (default),
#'   function will attempt to establish connection. @seealso [establish_db_con()]
#'
#' @return List with two dataframes:
#'   \itemize{
#'     \item `$estimates`: Main estimates data with catch groups, dates, and data grades
#'     \item `$analysis_metadata`: Analysis-level metadata including upload dates and creators
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get latest estimates for a fishery across multiple years
#' temp <- get_fishery_estimates(
#'   fishery_names = "Nisqually",
#'   years = 2021:2023,
#'   max_upload_date = TRUE
#' )
#'
#' # Get all estimates for specific fishery-year combinations
#' temp <- get_fishery_estimates(
#'   fishery_names = c("Skagit 2021", "Skagit 2022"),
#'   max_upload_date = FALSE
#' )
#'
#' # Get latest estimates with custom connection
#' con <- establish_db_con()
#' temp <- get_fishery_estimates(
#'   fishery_names = "Cascade",
#'   years = 2024,
#'   con = con
#' )
#' }

get_fishery_estimates <- function(fishery_names,
                                  years = NULL,
                                  max_upload_date = TRUE,
                                  con = NULL) {

  # Input validation ----
  if (missing(fishery_names) || length(fishery_names) == 0) {
    message("ERROR: 'fishery_names' must be provided as a non-empty character vector.")
    return(NULL)
  }

  if (!is.character(fishery_names)) {
    message("ERROR: 'fishery_names' must be a character vector.")
    return(NULL)
  }

  if (!is.null(years) && !is.numeric(years)) {
    message("ERROR: 'years' must be NULL or a numeric vector.")
    return(NULL)
  }

  if (!is.logical(max_upload_date) || length(max_upload_date) != 1) {
    message("ERROR: 'max_upload_date' must be a single logical value (TRUE or FALSE).")
    return(NULL)
  }

  # Build fishery names ----
  if (!is.null(years)) {
    fisheries <- as.character(interaction(fishery_names, years, sep = " "))
    message("Querying estimates for ", length(fisheries), " fishery-year combination(s)")
  } else {
    fisheries <- fishery_names
    message("Querying estimates for ", length(fisheries), " fishery name(s)")
  }

  # Establish database connection ----
  close_con_on_exit <- FALSE

  if (is.null(con)) {
    message("No connection provided. Establishing database connection...")
    con <- try(creelutils::establish_db_con(), silent = TRUE)

    if (inherits(con, "try-error")) {
      message("ERROR: Failed to establish database connection.")
      message("Ensure database connection is valid or provide a connection via 'con' parameter.")
      return(NULL)
    }
    close_con_on_exit <- TRUE
  }

  if (!DBI::dbIsValid(con)) {
    message("ERROR: Invalid database connection provided.")
    return(NULL)
  }

  # Ensure connection closes if we opened it
  if (close_con_on_exit) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  # Query analysis_lut ----
  message("Querying analysis metadata from database...")

  analysis_lut <- try(
    creelutils::fetch_db_table(con, "creel", "model_analysis_lut"),
    silent = TRUE
  )

  if (inherits(analysis_lut, "try-error")) {
    message("ERROR: Failed to query analysis_lut table.")
    message("Check database connection and table access.")
    return(NULL)
  }

  analysis_lut <- analysis_lut |>
    dplyr::mutate(
      fishery_name = stringr::str_extract(.data$analysis_name, "^[^_]+"),
      upload_date = lubridate::as_date(.data$created_datetime)
    ) |>
    dplyr::relocate(.data$fishery_name, .before = "analysis_name") |>
    dplyr::filter(.data$fishery_name %in% fisheries)

  if (nrow(analysis_lut) == 0) {
    message("WARNING: No analysis records found for specified fisheries.")
  } else {
    message("Found ", nrow(analysis_lut), " analysis record(s)")
  }

  # Query estimates ----
  message("Querying model estimates from database...")

  estimates <- try(
    creelutils::fetch_db_table(con, "creel", "vw_model_estimates_total"),
    silent = TRUE
  )

  if (inherits(estimates, "try-error")) {
    message("ERROR: Failed to query vw_model_estimates_total.")
    message("Check database connection and view access.")
    return(NULL)
  }

  # Filter to requested fisheries
  estimates <- estimates |>
    dplyr::mutate(fishery_name = stringr::str_extract(.data$analysis_name, "^[^_]+")) |>
    dplyr::filter(.data$fishery_name %in% fisheries)

  if (nrow(estimates) == 0) {
    message("WARNING: No estimates found for specified fisheries.")
    message("Returning NULL. Check fishery names or database content.")
    return(NULL)
  } else {
    message("Found ", nrow(estimates), " estimate record(s)")
  }

  # Format estimates ----
  message("Formatting estimate data...")

  estimates_formatted <- estimates |>
    dplyr::mutate(
      # Standardize fin_mark_desc to short codes
      fin_mark_desc = dplyr::case_when(
        .data$fin_mark_desc == "Adclip clip + No other external marks" ~ "AD",
        .data$fin_mark_desc == "No Adclip + No Other external marks" ~ "UM",
        .data$fin_mark_desc == "Unknown Adipose + Unknown fin clip" ~ "UNK",
        TRUE ~ .data$fin_mark_desc
      ),
      # Create catch_group from biological characteristics
      catch_group = dplyr::case_when(
        # If species_name is NA, it's an effort estimate
        is.na(.data$species_name) ~ "effort",
        # Otherwise combine all characteristics
        TRUE ~ paste(.data$species_name, .data$life_stage_name,
                     .data$fin_mark_desc, .data$fate_name, sep = "_")
      )
    )

  message("Formatted ", nrow(estimates_formatted), " estimate records")

  # Join analysis metadata ----
  message("Joining analysis metadata to estimates...")

  # Identify columns that will conflict in the join
  estimates_cols <- names(estimates_formatted)
  analysis_cols <- names(analysis_lut)

  # Find overlapping columns besides join keys
  potential_conflicts <- intersect(estimates_cols, analysis_cols)
  potential_conflicts <- setdiff(potential_conflicts, c("analysis_id", "fishery_name"))

  if (length(potential_conflicts) > 0) {
    message("Note: Removing duplicate columns from analysis_lut: ",
            paste(potential_conflicts, collapse = ", "))
  }

  # Select only columns that exist and won't create duplicates
  analysis_sub <- analysis_lut |>
    dplyr::select(dplyr::any_of(c("fishery_name", "analysis_id", "upload_date", "created_by")))

  # Select all relevant columns from estimates (keep analysis_name from estimates)
  estimates_sub <- estimates_formatted |>
    dplyr::select(dplyr::any_of(c(
      # Identifiers
      "analysis_id", "analysis_name", "project_id", "project_name", "fishery_name",
      # Biological characteristics
      "species_name", "life_stage_name", "fate_name", "fin_mark_desc",
      # Estimate details
      "model_type", "estimate_type", "estimate_value", "estimate_category",
      # Time period
      "period", "timestep", "min_event_date", "max_event_date",
      # Quality/metadata
      "data_grade",
      # Derived field
      "catch_group"
    )))

  # Determine join keys (only use keys that exist in both tables)
  join_keys <- "analysis_id"
  if ("fishery_name" %in% names(analysis_sub) && "fishery_name" %in% names(estimates_sub)) {
    join_keys <- c("analysis_id", "fishery_name")
  }

  combined <- try(
    estimates_sub |>
      dplyr::left_join(analysis_sub, by = join_keys) |>
      dplyr::arrange(dplyr::desc(.data$upload_date), .data$catch_group),
    silent = TRUE
  )

  if (inherits(combined, "try-error")) {
    message("ERROR: Failed to join estimates and analysis metadata.")
    message("Join keys used: ", paste(join_keys, collapse = ", "))
    return(NULL)
  }

  if (nrow(combined) == 0) {
    message("WARNING: No records after joining estimates and analysis metadata.")
  } else {
    message("Successfully joined ", nrow(combined), " record(s)")
  }

  # Final check: remove any .x or .y columns that slipped through
  duplicate_cols <- grep("\\.(x|y)$", names(combined), value = TRUE)
  if (length(duplicate_cols) > 0) {
    message("Cleaning up duplicate columns: ", paste(duplicate_cols, collapse = ", "))

    # For each .x column, rename it to original name if .y exists
    base_names <- unique(gsub("\\.(x|y)$", "", duplicate_cols))
    for (base_name in base_names) {
      if (paste0(base_name, ".x") %in% names(combined)) {
        combined <- combined |>
          dplyr::rename(!!base_name := !!paste0(base_name, ".x"))
      }
      # Remove .y version
      if (paste0(base_name, ".y") %in% names(combined)) {
        combined <- combined |>
          dplyr::select(-!!paste0(base_name, ".y"))
      }
    }
  }

  # Filter to max upload date if requested ----
  if (max_upload_date) {
    message("Filtering to maximum upload date per fishery and catch group...")

    final_estimates <- combined |>
      dplyr::group_by(.data$fishery_name, .data$catch_group) |>
      dplyr::filter(.data$upload_date == max(.data$upload_date, na.rm = TRUE)) |>
      dplyr::ungroup()

    message("Filtered to ", nrow(final_estimates), " most recent estimate(s)")
  } else {
    message("Returning all estimates (max_upload_date = FALSE)")
    final_estimates <- combined
  }

  # Create analysis metadata table ----
  analysis_metadata <- analysis_lut |>
    dplyr::select(dplyr::any_of(c("fishery_name", "analysis_id", "analysis_name",
                                  "upload_date", "created_by", "created_datetime")))

  # Ensure no duplicate columns in metadata either
  duplicate_meta_cols <- grep("\\.(x|y)$", names(analysis_metadata), value = TRUE)
  if (length(duplicate_meta_cols) > 0) {
    message("Cleaning up duplicate columns in metadata: ", paste(duplicate_meta_cols, collapse = ", "))

    base_names <- unique(gsub("\\.(x|y)$", "", duplicate_meta_cols))
    for (base_name in base_names) {
      if (paste0(base_name, ".x") %in% names(analysis_metadata)) {
        analysis_metadata <- analysis_metadata |>
          dplyr::rename(!!base_name := !!paste0(base_name, ".x"))
      }
      if (paste0(base_name, ".y") %in% names(analysis_metadata)) {
        analysis_metadata <- analysis_metadata |>
          dplyr::select(-!!paste0(base_name, ".y"))
      }
    }
  }

  # Summary message ----
  message("Successfully retrieved estimates for ",
          length(unique(final_estimates$fishery_name)), " fishery name(s)")

  if (max_upload_date) {
    message("Returning most recent estimates only")
  } else {
    message("Returning all available estimates")
  }

  # Return results ----
  return(list(
    estimates = final_estimates,
    analysis_metadata = analysis_metadata
  ))
}
