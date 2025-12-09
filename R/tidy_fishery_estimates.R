#' Convert fishery estimates from long to wide (tidy) format
#'
#' @description Separates estimates into catch and effort tables, then pivots each
#' so that each unique combination of analysis_id and catch_group has one row with
#' all estimate types as separate columns. Estimate types are prefixed with model
#' type (PE or BSS) to distinguish the source.
#'
#' Available estimate types in data:
#' - estimate_sum
#' - totaldaysopen
#' - totalobs
#' - mean
#' - standard_error_mean
#' - standard_deviation
#' - quantile_lower_2_5
#' - quantile_lower_25
#' - quantile_median_50
#' - quantile_upper_75
#' - quantile_upper_97_5
#' - n_eff
#' - R_hat
#' - n_div
#'
#' @family data_formatting
#' @param estimates_df A dataframe of estimates, typically from `get_fishery_estimates()$estimates`
#'
#' @return A list with two tibbles:
#'   \itemize{
#'     \item `$catch`: Wide format catch estimates (one row per analysis_id + catch_group)
#'     \item `$effort`: Wide format effort estimates (one row per analysis_id)
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get estimates
#' nisqually_est <- get_fishery_estimates(
#'   fishery_names = "Nisqually salmon",
#'   years = 2022
#' )
#'
#' # Convert to tidy format
#' tidy_est <- tidy_fishery_estimates(nisqually_est$estimates)
#'
#' # Access catch and effort separately
#' catch_data <- tidy_est$catch
#' effort_data <- tidy_est$effort
#' }

tidy_fishery_estimates <- function(estimates_df) {

  # Input validation ----
  if (missing(estimates_df) || !is.data.frame(estimates_df)) {
    message("ERROR: 'estimates_df' must be a dataframe.")
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

  # Return results ----
  message("Successfully created tidy estimates")

  return(list(
    catch = catch_wide,
    effort = effort_wide
  ))
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
      c("catch_group", "species_name", "life_stage_name", "fate_name", "fin_mark_desc"),
      names(df)
    )
  } else {
    bio_cols <- character(0)
  }

  meta_cols <- intersect(
    c("period", "timestep", "min_event_date", "max_event_date",
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
#' tidy_est <- tidy_fishery_estimates(est$estimates)
#'
#' # Extract only PE estimates from catch
#' pe_catch <- extract_estimate_types(tidy_est$catch, "^PE_")
#'
#' # Extract only BSS mean and CI from catch
#' bss_summary <- extract_estimate_types(
#'   tidy_est$catch,
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
