#' Obtain and format creel estimates for sets of fishery-years
#'
#' @description Query model estimates from the creel database for specified fisheries and years.
#' Returns formatted estimate data split into catch and effort tables at total and/or stratum
#' level, along with analysis metadata. Based on approach used in `get_fishery_data()`.
#'
#' @family internal_data
#' @param fishery_names Character string of fishery name or vector of character strings.
#'   If `years` is provided, `fishery_names` is combined with `year` to create fishery names.
#'   If `years` is not provided, uses this character string or vector of character strings as
#'   the exact fishery names. Try `search_fishery_name()` to see available options.
#' @param years Integer or vector of integers identifying years of data to pull.
#'   Optional argument, defaults to NULL.
#' @param temporal_agg Character string specifying the temporal aggregation level of estimates
#'   to retrieve. One of `"total"` (fishery-period totals only), `"stratum"` (stratum-level
#'   estimates only), or `"all"` (both total and stratum). Defaults to `"all"`.
#' @param max_upload_date Logical. If TRUE (default), filters to only the most recent upload
#'   date for each fishery_name and catch_group combination. If FALSE, returns all estimates
#'   for each fishery.
#' @param query_timeout Numeric. Maximum number of seconds to wait for each database query
#'   before cancelling. Applies a PostgreSQL `statement_timeout` server-side, so the limit
#'   is enforced even if R cannot interrupt the call. If NULL (default), no timeout is set.
#'   Useful for detecting hung queries; raise the value if legitimate queries are being
#'   cancelled (stratum queries against large fisheries can take 30–120 s).
#' @param con A valid connection to the WDFW PostgreSQL database. If NULL (default),
#'   function will attempt to establish connection. @seealso [connect_creel_db()]
#'
#' @return Named list. Tables present depend on `temporal_agg`:
#'   \itemize{
#'     \item `$catch_total`: Total-level catch estimates (present when `temporal_agg` is
#'       `"total"` or `"all"`)
#'     \item `$effort_total`: Total-level effort estimates (present when `temporal_agg` is
#'       `"total"` or `"all"`)
#'     \item `$catch_stratum`: Stratum-level catch estimates (present when `temporal_agg` is
#'       `"stratum"` or `"all"`)
#'     \item `$effort_stratum`: Stratum-level effort estimates (present when `temporal_agg` is
#'       `"stratum"` or `"all"`)
#'     \item `$catchrate_stratum`: Stratum-level CPUE estimates (BSS only; present when
#'       `temporal_agg` is `"stratum"` or `"all"`)
#'     \item `$analysis_metadata`: Analysis-level metadata including upload dates and creators
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get latest total and stratum estimates for a fishery across multiple years
#' temp <- get_fishery_estimates(
#'   fishery_names = "Nisqually salmon",
#'   years = 2021:2023,
#'   temporal_agg = "all",
#'   max_upload_date = TRUE
#' )
#' temp$catch_total
#' temp$catch_stratum
#'
#' # Get only total estimates for specific fishery-year combinations
#' temp <- get_fishery_estimates(
#'   fishery_names = c("Skagit 2021", "Skagit 2022"),
#'   temporal_agg = "total",
#'   max_upload_date = FALSE
#' )
#'
#' # Get only stratum estimates with a custom connection
#' con <- connect_creel_db()
#' temp <- get_fishery_estimates(
#'   fishery_names = "Cascade",
#'   years = 2024,
#'   temporal_agg = "stratum",
#'   con = con
#' )
#' }

get_fishery_estimates <- function(fishery_names,
                                  years = NULL,
                                  temporal_agg = "all",
                                  max_upload_date = TRUE,
                                  query_timeout = NULL,
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

  if (!is.character(temporal_agg) || length(temporal_agg) != 1 ||
      !temporal_agg %in% c("total", "stratum", "all")) {
    message("ERROR: 'temporal_agg' must be one of: 'total', 'stratum', or 'all'.")
    return(NULL)
  }

  if (!is.logical(max_upload_date) || length(max_upload_date) != 1 ||
      is.na(max_upload_date)) {
    message("ERROR: 'max_upload_date' must be a single logical value (TRUE or FALSE).")
    return(NULL)
  }

  if (!is.null(query_timeout) &&
      (!is.numeric(query_timeout) || length(query_timeout) != 1 ||
       is.na(query_timeout) || query_timeout <= 0)) {
    message("ERROR: 'query_timeout' must be NULL or a single positive number (seconds).")
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
    con <- try(creelutils::connect_creel_db(), silent = TRUE)

    if (inherits(con, "try-error")) {
      message("ERROR: Failed to establish database connection.")
      message("Ensure database connection is valid or provide a connection via 'con' parameter.")
      return(NULL)
    }
    close_con_on_exit <- TRUE
  }

  con_valid <- tryCatch(DBI::dbIsValid(con), error = function(e) FALSE)
  if (!con_valid) {
    message("ERROR: Invalid database connection provided.")
    return(NULL)
  }

  # Ensure connection closes if we opened it
  if (close_con_on_exit) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  # Query analysis_lut ----
  cli::cli_progress_step("Querying analysis metadata")

  analysis_lut <- try(
    creelutils::fetch_db_table(con, "creel", "model_analysis_lut"),
    silent = TRUE
  )

  if (inherits(analysis_lut, "try-error")) {
    cli::cli_progress_done(result = "failed")
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
    cli::cli_progress_done(result = "failed")
    message("WARNING: No analysis records found for specified fisheries.")
  } else {
    cli::cli_progress_update(
      status = paste0("Found ", nrow(analysis_lut), " analysis record(s)")
    )
  }

  # Collect the relevant analysis IDs upfront — used to filter estimates server-side
  relevant_ids <- analysis_lut$analysis_id

  if (length(relevant_ids) == 0) {
    cli::cli_progress_done(result = "failed")
    message("WARNING: No analysis IDs found. Cannot query estimates.")
    return(list(
      analysis_metadata = dplyr::select(
        analysis_lut,
        dplyr::any_of(c("fishery_name", "analysis_id", "analysis_name",
                        "upload_date", "created_by", "created_datetime"))
      )
    ))
  }

  # Query estimates ----
  message("Querying model estimates from database...")

  # -- Internal: apply/reset PostgreSQL statement_timeout (silently no-ops on unsupported drivers)
  .set_timeout <- function(secs) {
    if (is.null(secs)) return(invisible(NULL))
    tryCatch(
      DBI::dbExecute(con, paste0("SET statement_timeout = ", as.integer(secs * 1000))),
      error = function(e) {
        message("WARNING: Could not apply query timeout: ", conditionMessage(e))
      }
    )
  }

  .reset_timeout <- function() {
    tryCatch(
      DBI::dbExecute(con, "SET statement_timeout = 0"),
      error = function(e) invisible(NULL)
    )
  }

  .is_timeout_error <- function(msg) {
    grepl("statement timeout|canceling statement|timed out|timeout",
          msg, ignore.case = TRUE)
  }

  # -- Helper: format a raw estimates dataframe (standardize fin_mark_desc -> fin_mark, derive catch_group)
  .format_estimates <- function(df) {
    df |>
      dplyr::mutate(
        fishery_name = stringr::str_extract(.data$analysis_name, "^[^_]+"),
        fin_mark = dplyr::case_when(
          .data$fin_mark_desc == "Adclip clip + No other external marks" ~ "AD",
          .data$fin_mark_desc == "No Adclip + No Other external marks" ~ "UM",
          .data$fin_mark_desc == "Unknown Adipose + Unknown fin clip" ~ "UNK",
          TRUE ~ .data$fin_mark_desc
        ),
        catch_group = dplyr::case_when(
          is.na(.data$species_name) ~ "effort",
          TRUE ~ paste(.data$species_name, .data$life_stage_name,
                       .data$fin_mark, .data$fate_name, sep = "_")
        )
      ) |>
      dplyr::filter(.data$fishery_name %in% fisheries)
  }

  # -- Helper: join analysis metadata, clean duplicates, filter max_upload_date, split catch/effort
  .finalize_estimates <- function(formatted_df, extra_cols = character(0)) {

    analysis_sub <- analysis_lut |>
      dplyr::select(dplyr::any_of(c("fishery_name", "analysis_id", "upload_date", "created_by")))

    base_cols <- c(
      "analysis_id", "analysis_name", "project_id", "project_name", "fishery_name",
      "species_name", "life_stage_name", "fate_name", "fin_mark",
      "model_type", "estimate_type", "estimate_value", "estimate_category",
      "period", "min_event_date", "max_event_date",
      "data_grade", "catch_group"
    )

    estimates_sub <- formatted_df |>
      dplyr::select(dplyr::any_of(c(base_cols, extra_cols)))

    join_keys <- intersect(c("analysis_id", "fishery_name"),
                           intersect(names(analysis_sub), names(estimates_sub)))

    combined <- try(
      estimates_sub |>
        dplyr::left_join(analysis_sub, by = join_keys) |>
        dplyr::arrange(dplyr::desc(.data$upload_date), .data$catch_group),
      silent = TRUE
    )

    if (inherits(combined, "try-error")) {
      message("ERROR: Failed to join estimates and analysis metadata.")
      return(NULL)
    }

    # Remove any .x / .y duplicates that slipped through
    duplicate_cols <- grep("\\.(x|y)$", names(combined), value = TRUE)
    if (length(duplicate_cols) > 0) {
      base_names <- unique(gsub("\\.(x|y)$", "", duplicate_cols))
      for (base_name in base_names) {
        if (paste0(base_name, ".x") %in% names(combined)) {
          combined <- combined |>
            dplyr::rename(!!base_name := !!paste0(base_name, ".x"))
        }
        if (paste0(base_name, ".y") %in% names(combined)) {
          combined <- combined |>
            dplyr::select(-dplyr::all_of(paste0(base_name, ".y")))
        }
      }
    }

    # Filter to max upload date if requested
    if (max_upload_date) {
      combined <- combined |>
        dplyr::group_by(.data$fishery_name, .data$catch_group) |>
        dplyr::filter(.data$upload_date == max(.data$upload_date, na.rm = TRUE)) |>
        dplyr::ungroup()
    }

    # Split into catch, effort, and catchrate (CPUE)
    catch_out <- combined |>
      dplyr::filter(.data$estimate_category %in% c("catch", "C_sum"))

    effort_out <- combined |>
      dplyr::filter(.data$estimate_category %in% c("effort", "E_sum"))

    catchrate_out <- combined |>
      dplyr::filter(.data$estimate_category == "CPUE")

    list(catch = catch_out, effort = effort_out, catchrate = catchrate_out)
  }

  # -- Query total-level estimates --
  catch_total  <- NULL
  effort_total <- NULL

  if (temporal_agg %in% c("total", "all")) {
    cli::cli_progress_step("Querying total-level estimates (vw_model_estimates_total)")

    t0 <- proc.time()[["elapsed"]]
    .set_timeout(query_timeout)
    raw_total <- tryCatch(
      dplyr::tbl(con, dbplyr::in_schema("creel", "vw_model_estimates_total")) |>
        dplyr::filter(.data$analysis_id %in% !!relevant_ids) |>
        dplyr::collect(),
      error = function(e) structure(list(message = conditionMessage(e)), class = "query_error")
    )
    .reset_timeout()
    t_elapsed <- round(proc.time()[["elapsed"]] - t0, 1)

    if (inherits(raw_total, "query_error")) {
      cli::cli_progress_done(result = "failed")
      if (.is_timeout_error(raw_total$message)) {
        message("ERROR: Query timed out after ", t_elapsed, "s (limit = ", query_timeout, "s).")
        message("Consider increasing 'query_timeout' or narrowing 'fishery_names'/years.")
      } else {
        message("ERROR: Query failed after ", t_elapsed, "s. Details: ", raw_total$message)
      }
      return(NULL)
    }
    message("  Query completed in ", t_elapsed, "s")

    formatted_total <- .format_estimates(raw_total)

    if (nrow(formatted_total) == 0) {
      cli::cli_progress_done(result = "failed")
      message("WARNING: No total-level estimates found for specified fisheries.")
    } else {
      # total view uses 'timestep' column
      split_total  <- .finalize_estimates(formatted_total, extra_cols = "timestep")
      catch_total  <- split_total$catch
      effort_total <- split_total$effort
      cli::cli_progress_update(
        status = paste0(nrow(catch_total), " catch / ", nrow(effort_total), " effort records (", t_elapsed, "s)")
      )
    }
  }

  # -- Query stratum-level estimates --
  catch_stratum     <- NULL
  effort_stratum    <- NULL
  catchrate_stratum <- NULL

  if (temporal_agg %in% c("stratum", "all")) {
    cli::cli_progress_step("Querying stratum-level estimates (vw_model_estimates_stratum)")

    t0 <- proc.time()[["elapsed"]]
    .set_timeout(query_timeout)
    raw_stratum <- tryCatch(
      dplyr::tbl(con, dbplyr::in_schema("creel", "vw_model_estimates_stratum")) |>
        dplyr::filter(.data$analysis_id %in% !!relevant_ids) |>
        dplyr::collect(),
      error = function(e) structure(list(message = conditionMessage(e)), class = "query_error")
    )
    .reset_timeout()
    t_elapsed <- round(proc.time()[["elapsed"]] - t0, 1)

    if (inherits(raw_stratum, "query_error")) {
      cli::cli_progress_done(result = "failed")
      if (.is_timeout_error(raw_stratum$message)) {
        message("ERROR: Query timed out after ", t_elapsed, "s (limit = ", query_timeout, "s).")
        message("Consider increasing 'query_timeout', narrowing 'fishery_names'/years, or using temporal_agg = \"total\".")
      } else {
        message("ERROR: Query failed after ", t_elapsed, "s. Details: ", raw_stratum$message)
      }
      return(NULL)
    }
    message("  Query completed in ", t_elapsed, "s")

    formatted_stratum <- .format_estimates(raw_stratum)

    if (nrow(formatted_stratum) == 0) {
      cli::cli_progress_done(result = "failed")
      message("WARNING: No stratum-level estimates found for specified fisheries.")
    } else {
      # stratum view uses additional grouping columns
      stratum_extra <- c("angler_type_name", "catch_area_code", "section_num",
                         "day_type", "period_timestep")
      split_stratum     <- .finalize_estimates(formatted_stratum, extra_cols = stratum_extra)
      catch_stratum     <- split_stratum$catch
      effort_stratum    <- split_stratum$effort
      catchrate_stratum <- split_stratum$catchrate
      cli::cli_progress_update(
        status = paste0(
          nrow(catch_stratum), " catch / ",
          nrow(effort_stratum), " effort / ",
          nrow(catchrate_stratum), " catchrate records (", t_elapsed, "s)"
        )
      )
    }
  }

  # Create analysis metadata table ----
  analysis_metadata <- analysis_lut |>
    dplyr::select(dplyr::any_of(c("fishery_name", "analysis_id", "analysis_name",
                                  "upload_date", "created_by", "created_datetime")))

  # Summary ----
  n_fisheries <- length(fisheries)
  cli::cli_progress_done()
  cli::cli_alert_success(
    "Retrieved estimates for {n_fisheries} fishery name{?s} \
({if (max_upload_date) 'most recent only' else 'all uploads'})"
  )

  # Return results — only include tables relevant to temporal_agg ----
  out <- list(analysis_metadata = analysis_metadata)

  if (temporal_agg %in% c("total", "all")) {
    out$catch_total  <- catch_total
    out$effort_total <- effort_total
  }

  if (temporal_agg %in% c("stratum", "all")) {
    out$catch_stratum     <- catch_stratum
    out$effort_stratum    <- effort_stratum
    out$catchrate_stratum <- catchrate_stratum
  }

  return(out)
}
