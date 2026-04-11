#' Convert fishery estimates from long to wide (tidy) format
#'
#' @description Accepts either the full list returned by `get_fishery_estimates()` or
#' a single estimates dataframe. Pivots each table so that each unique observation
#' has one row with all estimate types as separate columns.
#'
#' **Total-level tables** (`catch_total`, `effort_total`): PE and BSS rows are
#' coalesced into a single row per observation; estimate type columns are prefixed
#' with the model type (`PE_estimate_sum`, `BSS_mean`, etc.).
#'
#' **Stratum-level tables** are split by model type because PE and BSS operate on
#' different temporal groupings and cannot be meaningfully coalesced. Each model
#' gets its own table with bare `estimate_type` names (no redundant prefix):
#' `catch_stratum_pe`, `catch_stratum_bss`, `effort_stratum_pe`, `effort_stratum_bss`.
#'
#' `analysis_metadata` is passed through unchanged.
#'
#' When a single dataframe is provided (legacy use), the function splits it into
#' catch and effort and returns `list(catch = ..., effort = ...)`.
#'
#' @family data_formatting
#' @param estimates A named list from `get_fishery_estimates()`, or a single
#'   dataframe of estimates (legacy use). When a list is provided, recognized
#'   table keys are `catch_total`, `effort_total`, `catch_stratum`, `effort_stratum`.
#'
#' @return A named list:
#'   \itemize{
#'     \item `$catch_total` Wide format, PE and BSS coalesced, prefixed columns
#'     \item `$effort_total` Wide format, PE and BSS coalesced, prefixed columns
#'     \item `$catch_stratum_pe` Wide format, PE stratum catch, bare column names
#'     \item `$catch_stratum_bss` Wide format, BSS stratum catch, bare column names
#'     \item `$effort_stratum_pe` Wide format, PE stratum effort, bare column names
#'     \item `$effort_stratum_bss` Wide format, BSS stratum effort, bare column names
#'     \item `$catchrate_stratum_bss` Wide format, BSS stratum CPUE, bare column names
#'     \item `$analysis_metadata` Passed through unchanged
#'     \item When input is a dataframe: `list(catch = ..., effort = ...)`
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get estimates for total and stratum levels
#' est <- get_fishery_estimates(
#'   fishery_names = "Nisqually salmon",
#'   years = 2022,
#'   temporal_agg = "all"
#' )
#'
#' # Convert full list to tidy format
#' tidy_est <- tidy_fishery_estimates(est)
#' tidy_est$catch_total        # PE + BSS coalesced, prefixed columns
#' tidy_est$catch_stratum_pe   # PE stratum, bare column names
#' tidy_est$catch_stratum_bss  # BSS stratum, bare column names
#'
#' # Legacy single-dataframe use
#' tidy_total <- tidy_fishery_estimates(est$catch_total)
#' }

tidy_fishery_estimates <- function(estimates) {

  # Input validation and dispatch ----
  if (missing(estimates)) {
    message("ERROR: 'estimates' must be a dataframe or a named list from get_fishery_estimates().")
    return(NULL)
  }

  # Named list path: dispatch each recognized table, pass metadata through
  recognized_catch   <- c("catch_total", "catch_stratum")
  recognized_effort  <- c("effort_total", "effort_stratum")
  recognized_stratum <- c("catch_stratum", "effort_stratum", "catchrate_stratum")
  recognized_all     <- c(recognized_catch, recognized_effort, "catchrate_stratum", "analysis_metadata")

  if (is.list(estimates) && !is.data.frame(estimates) &&
      any(names(estimates) %in% recognized_all)) {
    message("Input detected as named list from get_fishery_estimates()")

    out <- list()

    for (tbl_name in names(estimates)) {
      if (tbl_name == "analysis_metadata") {
        out$analysis_metadata <- estimates$analysis_metadata
        next
      }

      if (!tbl_name %in% recognized_all) {
        message("Skipping unrecognized list element: '", tbl_name, "'")
        next
      }

      df <- estimates[[tbl_name]]

      if (is.null(df) || nrow(df) == 0) {
        message("Skipping empty table: '", tbl_name, "'")
        next
      }

      # Stratum tables: split by model_type, one wide table per model
      if (tbl_name %in% recognized_stratum) {
        include_cg <- tbl_name %in% c("catch_stratum", "catchrate_stratum")
        model_types <- sort(unique(df$model_type))

        if (length(model_types) == 0) {
          message("WARNING: No model_type values found in '", tbl_name, "'. Skipping.")
          next
        }

        for (mt in model_types) {
          out_key <- paste0(tbl_name, "_", tolower(mt))
          df_mt   <- df |> dplyr::filter(.data$model_type == mt)
          message("Pivoting '", out_key, "' (", nrow(df_mt), " rows)...")
          out[[out_key]] <- .pivot_single_model_wide(df_mt,
                                                     include_catch_group = include_cg) |>
            .add_date_fields()
        }
        next
      }

      # Total tables: coalesce PE + BSS into one row, prefixed columns
      include_cg <- tbl_name %in% recognized_catch
      message("Pivoting '", tbl_name, "' (", nrow(df), " rows)...")
      out[[tbl_name]] <- .pivot_estimates_wide(df, include_catch_group = include_cg) |>
        .add_date_fields()
    }

    message("Successfully created tidy estimates")
    return(out)
  }

  # Single dataframe path (legacy) ----
  estimates_df <- estimates

  if (!is.data.frame(estimates_df)) {
    message("ERROR: 'estimates' must be a dataframe or a named list from get_fishery_estimates().")
    return(NULL)
  }

  required_cols <- c("analysis_id", "estimate_type", "estimate_value", "estimate_category")
  missing_cols <- setdiff(required_cols, names(estimates_df))

  if (length(missing_cols) > 0) {
    message("ERROR: Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }

  if (nrow(estimates_df) == 0) {
    message("WARNING: Input dataframe is empty.")
    return(list(catch = NULL, effort = NULL))
  }

  message("Converting ", nrow(estimates_df), " rows to tidy format...")

  # Classify into catch vs effort based on estimate_category ----
  estimates_classified <- estimates_df |>
    dplyr::mutate(
      estimate_class = dplyr::case_when(
        estimate_category %in% c("catch", "C_sum") ~ "catch",
        estimate_category %in% c("effort", "E_sum") ~ "effort",
        TRUE ~ NA_character_
      )
    )

  # Check for unclassified records
  unclassified <- estimates_classified |>
    dplyr::filter(is.na(estimate_class))

  if (nrow(unclassified) > 0) {
    unique_cats <- unique(unclassified$estimate_category)
    message("WARNING: ", nrow(unclassified), " rows with unrecognized estimate_category: ",
            paste(unique_cats, collapse = ", "))
    message("These records will be excluded from output.")
  }

  # Remove unclassified records
  estimates_classified <- estimates_classified |>
    dplyr::filter(!is.na(estimate_class))

  n_catch <- sum(estimates_classified$estimate_class == "catch")
  n_effort <- sum(estimates_classified$estimate_class == "effort")

  message("Classified ", n_catch, " catch records and ", n_effort, " effort records")

  # Add model type prefix to estimate_type ----
  estimates_prefixed <- estimates_classified |>
    dplyr::mutate(
      estimate_type_prefixed = paste(.data$model_type, .data$estimate_type, sep = "_")
    )

  # Split into catch and effort ----
  catch_long <- estimates_prefixed |>
    dplyr::filter(estimate_class == "catch")

  effort_long <- estimates_prefixed |>
    dplyr::filter(estimate_class == "effort")

  # Define grouping columns (exclude estimate-specific fields) ----
  exclude_cols <- c("estimate_type", "estimate_value", "estimate_category",
                    "estimate_class", "estimate_type_prefixed", "model_type")

  grouping_cols <- setdiff(names(estimates_prefixed), exclude_cols)

  # Pivot catch data wider ----
  message("Pivoting catch data to wide format...")

  catch_wide <- NULL
  if (nrow(catch_long) > 0) {
    catch_wide <- try(
      catch_long |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(grouping_cols),
          names_from = estimate_type_prefixed,
          values_from = estimate_value,
          values_fn = list(estimate_value = function(x) {
            if (length(x) > 1) {
              message("WARNING: Multiple values found for same estimate_type. Using first value.")
              return(x[1])
            }
            return(x)
          })
        ),
      silent = FALSE
    )

    if (inherits(catch_wide, "try-error")) {
      message("ERROR: Failed to pivot catch data to wide format.")
      message("Error details: ", attr(catch_wide, "condition")$message)
      message("Grouping columns used: ", paste(grouping_cols, collapse = ", "))
      message("Number of unique estimate types: ", length(unique(catch_long$estimate_type_prefixed)))
      catch_wide <- NULL
    } else {
      # Remove estimate_class if it exists
      if ("estimate_class" %in% names(catch_wide)) {
        catch_wide <- catch_wide |> dplyr::select(-estimate_class)
      }

      n_catch_rows_before <- nrow(catch_wide)

      # Collapse PE and BSS rows that differ only in model estimates
      message("Collapsing separate PE and BSS rows into single observations...")
      catch_wide <- collapse_model_rows(catch_wide)

      n_catch_rows <- nrow(catch_wide)
      n_catch_estimates <- length(setdiff(names(catch_wide),
                                          intersect(grouping_cols, names(catch_wide))))
      message("Catch table: ", n_catch_rows_before, " rows collapsed to ",
              n_catch_rows, " rows with ", n_catch_estimates, " estimate columns")
    }
  } else {
    message("No catch data to pivot.")
  }

  # Pivot effort data wider ----
  message("Pivoting effort data to wide format...")

  effort_wide <- NULL
  if (nrow(effort_long) > 0) {
    # For effort, catch_group should always be "effort", so we can simplify grouping
    effort_grouping_cols <- setdiff(grouping_cols, "catch_group")

    effort_wide <- try(
      effort_long |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(effort_grouping_cols),
          names_from = estimate_type_prefixed,
          values_from = estimate_value,
          values_fn = list(estimate_value = function(x) {
            if (length(x) > 1) {
              message("WARNING: Multiple values found for same estimate_type. Using first value.")
              return(x[1])
            }
            return(x)
          })
        ),
      silent = FALSE
    )

    if (inherits(effort_wide, "try-error")) {
      message("ERROR: Failed to pivot effort data to wide format.")
      message("Error details: ", attr(effort_wide, "condition")$message)
      message("Grouping columns used: ", paste(effort_grouping_cols, collapse = ", "))
      message("Number of unique estimate types: ", length(unique(effort_long$estimate_type_prefixed)))
      effort_wide <- NULL
    } else {
      # Remove estimate_class if it exists
      if ("estimate_class" %in% names(effort_wide)) {
        effort_wide <- effort_wide |> dplyr::select(-estimate_class)
      }

      n_effort_rows_before <- nrow(effort_wide)

      # Collapse PE and BSS rows that differ only in model estimates
      message("Collapsing separate PE and BSS rows into single observations...")
      effort_wide <- collapse_model_rows(effort_wide)

      n_effort_rows <- nrow(effort_wide)
      n_effort_estimates <- length(setdiff(names(effort_wide),
                                           intersect(effort_grouping_cols, names(effort_wide))))
      message("Effort table: ", n_effort_rows_before, " rows collapsed to ",
              n_effort_rows, " rows with ", n_effort_estimates, " estimate columns")
    }
  } else {
    message("No effort data to pivot.")
  }

  # Reorder columns for readability ----
  if (!is.null(catch_wide)) {
    catch_wide <- reorder_estimate_columns(catch_wide, include_catch_group = TRUE)
  }

  if (!is.null(effort_wide)) {
    effort_wide <- reorder_estimate_columns(effort_wide, include_catch_group = FALSE)
  }

  # Return results (legacy single-df path) ----
  message("Successfully created tidy estimates")

  return(list(
    catch  = .add_date_fields(catch_wide),
    effort = .add_date_fields(effort_wide)
  ))
}


#' Add derived date fields to a wide estimates table
#'
#' @description Internal helper that adds `year`, `month`, and `week` columns
#' derived from `min_event_date` (the start of each observation's time window).
#' If `min_event_date` is absent the table is returned unchanged.
#'
#' @param df A wide-format estimates dataframe
#' @return The same dataframe with `year`, `month`, and `week` columns appended
#'   before the estimate columns, or unchanged if `min_event_date` is missing.
#' @keywords internal

.add_date_fields <- function(df) {
  if (is.null(df) || !"min_event_date" %in% names(df)) {
    return(df)
  }

  df |>
    dplyr::mutate(
      year         = lubridate::year(.data$min_event_date),
      month        = lubridate::month(.data$min_event_date),
      week         = lubridate::week(.data$min_event_date),
      julian_date  = lubridate::yday(.data$min_event_date)
    ) |>
    dplyr::relocate("year", "month", "week", "julian_date", .after = "max_event_date")
}


#' Pivot a pre-split estimates dataframe to wide format
#'
#' @description Internal helper used by `tidy_fishery_estimates()`. Expects a
#' dataframe that is already restricted to either catch or effort records
#' (i.e., the `estimate_category` split has already been done upstream by
#' `get_fishery_estimates()`). Adds model-type prefix, pivots wider, collapses
#' PE/BSS rows, then reorders columns.
#'
#' @param df A long-format dataframe of either catch or effort estimates
#' @param include_catch_group Logical; TRUE for catch tables, FALSE for effort tables
#'
#' @return Wide-format dataframe, or NULL on failure
#' @keywords internal

.pivot_estimates_wide <- function(df, include_catch_group = TRUE) {

  required_cols <- c("analysis_id", "estimate_type", "estimate_value")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    message("ERROR: Missing required columns in table: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }

  # Use an explicit, curated set of observation-identity columns as id_cols.
  # These are guaranteed to be identical between PE and BSS rows for the same
  # observation. Deriving id_cols dynamically (all non-estimate cols) is the root
  # cause of PE/BSS rows landing in separate pivot rows: columns like data_grade
  # can differ between models and break the grouping key.
  # With matching id_cols, pivot_wider puts PE_xxx and BSS_xxx in the same row
  # automatically — no collapse step required.
  id_col_candidates <- c(
    "analysis_id", "analysis_name", "project_id", "project_name", "fishery_name",
    "period", "timestep", "min_event_date", "max_event_date",
    "upload_date", "created_by"
  )

  if (include_catch_group) {
    id_col_candidates <- c(
      id_col_candidates,
      "catch_group", "species_name", "life_stage_name", "fate_name", "fin_mark"
    )
  }

  id_cols <- intersect(id_col_candidates, names(df))

  # Prefix estimate_type with model_type so PE and BSS estimates become distinct
  # named columns (PE_mean, BSS_mean) within the same output row.
  df_prefixed <- df |>
    dplyr::mutate(
      estimate_type_prefixed = paste(.data$model_type, .data$estimate_type, sep = "_")
    )

  wide <- try(
    df_prefixed |>
      tidyr::pivot_wider(
        id_cols     = dplyr::all_of(id_cols),
        names_from  = estimate_type_prefixed,
        values_from = estimate_value,
        values_fn   = list(estimate_value = function(x) {
          if (length(x) > 1) {
            message("WARNING: Multiple values for same estimate_type. Using first value.")
            return(x[1])
          }
          return(x)
        })
      ),
    silent = FALSE
  )

  if (inherits(wide, "try-error")) {
    message("ERROR: Failed to pivot data to wide format.")
    message("Observation key columns used: ", paste(id_cols, collapse = ", "))
    return(NULL)
  }

  n_est_cols <- length(grep("^(PE_|BSS_)", names(wide)))
  message("  ", nrow(wide), " rows x ", n_est_cols, " estimate columns")

  wide <- reorder_estimate_columns(wide, include_catch_group = include_catch_group)

  return(wide)
}


#' Pivot a single-model-type stratum estimates dataframe to wide format
#'
#' @description Internal helper for stratum tables that have already been filtered
#' to a single model type (PE or BSS). Uses bare `estimate_type` values as column
#' names (no model-type prefix, since the table name already conveys the model).
#' Does not call `collapse_model_rows()` — with one model type there is nothing
#' to coalesce.
#'
#' @param df A long-format dataframe of stratum estimates for a single model type
#' @param include_catch_group Logical; TRUE for catch tables, FALSE for effort tables
#'
#' @return Wide-format dataframe, or NULL on failure
#' @keywords internal

.pivot_single_model_wide <- function(df, include_catch_group = TRUE) {

  required_cols <- c("analysis_id", "estimate_type", "estimate_value")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    message("ERROR: Missing required columns in table: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }

  # Explicit observation-identity columns for stratum data.
  # Stratum adds grouping dimensions: angler_type, area, section, day_type.
  # period_timestep replaces timestep for stratum-level time grouping.
  id_col_candidates <- c(
    "analysis_id", "analysis_name", "project_id", "project_name", "fishery_name",
    "period", "period_timestep", "min_event_date", "max_event_date",
    "angler_type_name", "catch_area_code", "section_num", "day_type",
    "upload_date", "created_by"
  )

  if (include_catch_group) {
    id_col_candidates <- c(
      id_col_candidates,
      "catch_group", "species_name", "life_stage_name", "fate_name", "fin_mark"
    )
  }

  id_cols <- intersect(id_col_candidates, names(df))

  wide <- try(
    df |>
      tidyr::pivot_wider(
        id_cols     = dplyr::all_of(id_cols),
        names_from  = estimate_type,
        values_from = estimate_value,
        values_fn   = list(estimate_value = function(x) {
          if (length(x) > 1) {
            message("WARNING: Multiple values for same estimate_type. Using first value.")
            return(x[1])
          }
          return(x)
        })
      ),
    silent = FALSE
  )

  if (inherits(wide, "try-error")) {
    message("ERROR: Failed to pivot stratum data to wide format.")
    message("Observation key columns used: ", paste(id_cols, collapse = ", "))
    return(NULL)
  }

  n_est_cols <- length(setdiff(names(wide), id_cols))
  message("  ", nrow(wide), " rows x ", n_est_cols, " estimate columns")

  wide <- reorder_estimate_columns(wide, include_catch_group = include_catch_group)

  return(wide)
}


#' Collapse PE and BSS rows into single observations
#'
#' @description Internal helper function that combines rows where PE estimates
#' are in one row with NA for BSS columns, and BSS estimates are in another row
#' with NA for PE columns. Coalesces these into single rows with both PE and BSS data.
#'
#' @param df Wide format dataframe with PE_ and BSS_ prefixed columns
#'
#' @return Dataframe with collapsed rows
#' @keywords internal

collapse_model_rows <- function(df) {

  # Identify metadata columns (non-estimate columns)
  estimate_cols <- grep("^(PE_|BSS_)", names(df), value = TRUE)
  metadata_cols <- setdiff(names(df), estimate_cols)

  if (length(estimate_cols) == 0) {
    # No estimate columns to collapse
    return(df)
  }

  # Group by all metadata columns and collapse estimate columns
  result <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(metadata_cols))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(estimate_cols),
        ~ {
          # Remove NAs and take the first non-NA value
          non_na_vals <- .x[!is.na(.x)]
          if (length(non_na_vals) == 0) {
            return(NA_real_)
          } else if (length(non_na_vals) == 1) {
            return(non_na_vals[1])
          } else {
            # If multiple non-NA values exist, take the first and warn
            # This shouldn't happen if data is structured correctly
            return(non_na_vals[1])
          }
        }
      ),
      .groups = "drop"
    )

  return(result)
}


#' Reorder columns in tidy estimates for better readability
#'
#' @description Internal helper function to organize columns logically
#'
#' @param df Dataframe to reorder
#' @param include_catch_group Logical, whether to include catch_group in bio columns
#'
#' @return Dataframe with reordered columns
#' @keywords internal

reorder_estimate_columns <- function(df, include_catch_group = TRUE) {

  # Define column order categories
  id_cols <- intersect(
    c("analysis_id", "analysis_name", "project_id", "project_name", "fishery_name"),
    names(df)
  )

  if (include_catch_group) {
    bio_cols <- intersect(
      c("catch_group", "species_name", "life_stage_name", "fate_name", "fin_mark"),
      names(df)
    )
  } else {
    bio_cols <- character(0)
  }

  meta_cols <- intersect(
    c("period", "timestep", "period_timestep", "day_type",
      "angler_type_name", "catch_area_code", "section_num",
      "min_event_date", "max_event_date",
      "data_grade", "upload_date", "created_by"),
    names(df)
  )

  # Everything else is estimate columns
  estimate_cols <- setdiff(names(df), c(id_cols, bio_cols, meta_cols))

  # Reorder
  df |>
    dplyr::select(
      dplyr::all_of(id_cols),
      dplyr::all_of(bio_cols),
      dplyr::all_of(meta_cols),
      dplyr::all_of(estimate_cols)
    )
}


#' Extract specific estimate types from tidy estimates
#'
#' @description Helper function to extract specific prefixed estimate types
#' from tidy catch or effort tables.
#'
#' @param tidy_table A catch or effort table from `tidy_fishery_estimates()`
#' @param estimate_patterns Character vector of patterns to match estimate column names.
#'   Can use regex patterns. Default extracts common summary statistics.
#'
#' @return A tibble with only the requested estimate columns plus all metadata
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get and tidy estimates
#' est <- get_fishery_estimates("Nisqually salmon", years = 2022)
#' tidy_est <- tidy_fishery_estimates(est)
#'
#' # Extract only PE estimates from total catch
#' pe_catch <- extract_estimate_types(tidy_est$catch_total, "^PE_")
#'
#' # Extract only BSS mean and CI from total catch
#' bss_summary <- extract_estimate_types(
#'   tidy_est$catch_total,
#'   c("BSS_mean", "BSS_quantile_lower_2_5", "BSS_quantile_upper_97_5")
#' )
#'
#' # Extract all estimate sums
#' sums <- extract_estimate_types(tidy_est$catch, "estimate_sum")
#' }

extract_estimate_types <- function(tidy_table, estimate_patterns = NULL) {

  if (!is.data.frame(tidy_table)) {
    message("ERROR: 'tidy_table' must be a dataframe.")
    return(NULL)
  }

  # Default patterns for common estimates
  if (is.null(estimate_patterns)) {
    estimate_patterns <- c(
      "estimate_sum$",
      "totaldaysopen$",
      "totalobs$",
      "mean$",
      "standard_error_mean$",
      "standard_deviation$",
      "quantile_lower_2_5$",
      "quantile_lower_25$",
      "quantile_median_50$",
      "quantile_upper_75$",
      "quantile_upper_97_5$",
      "n_eff$",
      "R_hat$",
      "n_div$"
    )
  }

  # Identify metadata columns (non-estimate columns)
  metadata_patterns <- c(
    "^analysis_", "^project_", "^fishery_", "^species_", "^life_stage_",
    "^fate_", "^fin_mark_", "^catch_group$", "^period$", "^timestep$",
    "^min_event_", "^max_event_", "^data_grade$", "^upload_", "^created_"
  )

  metadata_cols <- names(tidy_table)[
    sapply(names(tidy_table), function(col) {
      any(sapply(metadata_patterns, function(p) grepl(p, col)))
    })
  ]

  # Find estimate columns matching patterns
  all_estimate_cols <- setdiff(names(tidy_table), metadata_cols)

  keep_estimates <- character(0)
  for (pattern in estimate_patterns) {
    matches <- grep(pattern, all_estimate_cols, value = TRUE)
    keep_estimates <- c(keep_estimates, matches)
  }

  keep_estimates <- unique(keep_estimates)

  if (length(keep_estimates) == 0) {
    message("WARNING: No columns matched the provided patterns.")
    message("Available estimate columns: ", paste(all_estimate_cols, collapse = ", "))
    return(tidy_table)
  }

  message("Extracting ", length(keep_estimates), " estimate column(s)")

  result <- tidy_table |>
    dplyr::select(dplyr::all_of(c(metadata_cols, keep_estimates)))

  return(result)
}
