# Tests for tidy_fishery_estimates()
# Test file: tests/testthat/test-tidy_fishery_estimates.R

# Setup test data ----

# Helper function to create mock long-format estimates data (legacy single-df use)
create_mock_estimates_long <- function() {
  tibble::tibble(
    analysis_id = rep("uuid-001", 24),
    analysis_name = rep("Nisqually_2021_PE_final", 24),
    project_id = rep("proj-001", 24),
    project_name = rep("Test Project", 24),
    fishery_name = rep("Nisqually", 24),

    # Catch records (12 rows: 2 catch groups x 2 models x 3 estimate types)
    species_name = c(rep("Chinook", 6), rep("Steelhead", 6), rep(NA, 12)),
    life_stage_name = c(rep("Adult", 6), rep("Adult", 6), rep(NA, 12)),
    fin_mark_desc = c(rep("AD", 6), rep("UM", 6), rep(NA, 12)),
    fate_name = c(rep("Kept", 6), rep("Released", 6), rep(NA, 12)),
    catch_group = c(
      rep("Chinook_Adult_AD_Kept", 6),
      rep("Steelhead_Adult_UM_Released", 6),
      rep("effort", 12)
    ),

    model_type = rep(c("PE", "PE", "PE", "BSS", "BSS", "BSS"), 4),
    estimate_type = rep(c("estimate_sum", "mean", "standard_error_mean"), 8),
    estimate_value = c(
      100, 95, 5,
      105, 100, 6,
      50, 48, 3,
      52, 50, 4,
      1000, 980, 20,
      1050, 1000, 25,
      2000, 1950, 30,
      2100, 2050, 35
    ),
    estimate_category = c(rep("catch", 12), rep("effort", 12)),

    period = rep(1, 24),
    timestep = rep("week", 24),
    min_event_date = rep(as.Date("2021-01-01"), 24),
    max_event_date = rep(as.Date("2021-12-31"), 24),
    data_grade = rep("Approved", 24),
    upload_date = rep(as.Date("2021-12-15"), 24),
    created_by = rep("test_user", 24)
  )
}

# Helper to create mock pre-split list (matching get_fishery_estimates() output)
create_mock_estimates_list <- function() {
  all_data <- create_mock_estimates_long()

  catch_df <- all_data |> dplyr::filter(estimate_category == "catch")
  effort_df <- all_data |>
    dplyr::filter(estimate_category == "effort") |>
    dplyr::select(-catch_group)

  list(
    catch_total    = catch_df,
    effort_total   = effort_df,
    catch_stratum  = NULL,
    effort_stratum = NULL,
    analysis_metadata = tibble::tibble(
      fishery_name = "Nisqually",
      analysis_id  = "uuid-001",
      analysis_name = "Nisqually_2021_PE_final",
      upload_date  = as.Date("2021-12-15"),
      created_by   = "test_user"
    )
  )
}

# Helper to create mock list including stratum data (split by model type)
create_mock_estimates_list_with_stratum <- function() {
  base <- create_mock_estimates_list()

  # Stratum catch: 2 catch groups x 2 model types x 3 estimate types x 2 strata
  # columns match vw_model_estimates_stratum structure
  catch_stratum <- tibble::tibble(
    analysis_id      = rep("uuid-001", 24),
    analysis_name    = rep("Nisqually_2021_PE_final", 24),
    project_id       = rep("proj-001", 24),
    project_name     = rep("Test Project", 24),
    fishery_name     = rep("Nisqually", 24),
    species_name     = rep(c("Chinook", "Steelhead"), each = 12),
    life_stage_name  = rep("Adult", 24),
    fin_mark_desc    = rep(c("AD", "UM"), each = 12),
    fate_name        = rep(c("Kept", "Released"), each = 12),
    catch_group      = rep(c("Chinook_Adult_AD_Kept", "Steelhead_Adult_UM_Released"), each = 12),
    angler_type_name = rep("Bank Anglers", 24),
    catch_area_code  = rep("786", 24),
    section_num      = rep(1L, 24),
    day_type         = rep(c("weekday", "weekend"), 12),
    model_type       = rep(c(rep("PE", 6), rep("BSS", 6)), 2),
    estimate_type    = rep(c("number_observations", "catch_estimate_mean",
                             "catch_estimate_variance",
                             "number_observations", "catch_estimate_mean",
                             "catch_estimate_variance"), 4),
    estimate_value   = runif(24, 0, 100),
    estimate_category = rep("catch", 24),
    period           = rep(1L, 24),
    min_event_date   = rep(as.Date("2021-01-01"), 24),
    max_event_date   = rep(as.Date("2021-01-07"), 24),
    data_grade       = rep("Approved", 24),
    upload_date      = rep(as.Date("2021-12-15"), 24),
    created_by       = rep("test_user", 24),
    period_timestep  = rep("week", 24)
  )

  effort_stratum <- tibble::tibble(
    analysis_id      = rep("uuid-001", 12),
    analysis_name    = rep("Nisqually_2021_PE_final", 12),
    project_id       = rep("proj-001", 12),
    project_name     = rep("Test Project", 12),
    fishery_name     = rep("Nisqually", 12),
    species_name     = NA_character_,
    angler_type_name = rep("Bank Anglers", 12),
    catch_area_code  = rep("786", 12),
    section_num      = rep(1L, 12),
    day_type         = rep(c("weekday", "weekend"), 6),
    model_type       = rep(c(rep("PE", 6), rep("BSS", 6)), 1),
    estimate_type    = rep(c("number_interviews", "effort_estimate_mean",
                             "effort_estimate_variance",
                             "number_interviews", "effort_estimate_mean",
                             "effort_estimate_variance"), 2),
    estimate_value   = runif(12, 0, 500),
    estimate_category = rep("effort", 12),
    period           = rep(1L, 12),
    min_event_date   = rep(as.Date("2021-01-01"), 12),
    max_event_date   = rep(as.Date("2021-01-07"), 12),
    data_grade       = rep("Approved", 12),
    upload_date      = rep(as.Date("2021-12-15"), 12),
    created_by       = rep("test_user", 12),
    period_timestep  = rep("week", 12)
  )

  base$catch_stratum  <- catch_stratum
  base$effort_stratum <- effort_stratum
  base
}

# Input Validation Tests ----

test_that("tidy_fishery_estimates requires a dataframe or recognized list", {
  expect_message(
    result <- tidy_fishery_estimates(),
    "ERROR: 'estimates' must be a dataframe or a named list"
  )
  expect_null(result)

  expect_message(
    result <- tidy_fishery_estimates("not a dataframe"),
    "ERROR: 'estimates' must be a dataframe or a named list"
  )
  expect_null(result)

  # An unrecognized plain list (no recognized table names) is treated as a
  # non-dataframe and hits the legacy-path error
  expect_message(
    result <- tidy_fishery_estimates(list(a = 1, b = 2)),
    "ERROR: 'estimates' must be a dataframe or a named list"
  )
  expect_null(result)
})

test_that("tidy_fishery_estimates checks for required columns", {
  # Missing analysis_id
  bad_data <- create_mock_estimates_long() |>
    dplyr::select(-analysis_id)

  expect_message(
    result <- tidy_fishery_estimates(bad_data),
    "ERROR: Missing required columns: analysis_id"
  )
  expect_null(result)

  # Missing multiple columns
  bad_data2 <- create_mock_estimates_long() |>
    dplyr::select(-analysis_id, -estimate_type, -estimate_value)

  expect_message(
    result <- tidy_fishery_estimates(bad_data2),
    "ERROR: Missing required columns: analysis_id, estimate_type, estimate_value"
  )
  expect_null(result)
})

test_that("tidy_fishery_estimates handles empty dataframe", {
  empty_df <- create_mock_estimates_long()[0, ]

  expect_message(
    result <- tidy_fishery_estimates(empty_df),
    "WARNING: Input dataframe is empty"
  )

  expect_type(result, "list")
  expect_named(result, c("catch", "effort"))
  expect_null(result$catch)
  expect_null(result$effort)
})

# List-path Tests ----

test_that("tidy_fishery_estimates accepts full list from get_fishery_estimates", {
  mock_list <- create_mock_estimates_list()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  expect_type(result, "list")
  expect_true("catch_total" %in% names(result))
  expect_true("effort_total" %in% names(result))
  expect_true("analysis_metadata" %in% names(result))
})

test_that("tidy_fishery_estimates list-path produces wide catch_total table", {
  mock_list <- create_mock_estimates_list()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  expect_s3_class(result$catch_total, "data.frame")
  expect_true("catch_group" %in% names(result$catch_total))
  catch_cols <- names(result$catch_total)
  expect_true(any(grepl("^PE_", catch_cols)))
  expect_true(any(grepl("^BSS_", catch_cols)))
})

test_that("tidy_fishery_estimates list-path produces wide effort_total table", {
  mock_list <- create_mock_estimates_list()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  expect_s3_class(result$effort_total, "data.frame")
  expect_false("catch_group" %in% names(result$effort_total))
  effort_cols <- names(result$effort_total)
  expect_true(any(grepl("^PE_|^BSS_", effort_cols)))
})

test_that("tidy_fishery_estimates passes analysis_metadata through unchanged", {
  mock_list <- create_mock_estimates_list()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  expect_identical(result$analysis_metadata, mock_list$analysis_metadata)
})

test_that("tidy_fishery_estimates skips NULL stratum tables gracefully", {
  mock_list <- create_mock_estimates_list() # catch_stratum and effort_stratum are NULL
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  # NULL stratum tables are skipped entirely — keys will not be present
  expect_null(result$catch_stratum_pe)
  expect_null(result$catch_stratum_bss)
  expect_null(result$effort_stratum_pe)
  expect_null(result$effort_stratum_bss)
})

test_that("tidy_fishery_estimates splits stratum tables by model type", {
  mock_list <- create_mock_estimates_list_with_stratum()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  # Should produce separate PE and BSS stratum tables
  expect_s3_class(result$catch_stratum_pe,  "data.frame")
  expect_s3_class(result$catch_stratum_bss, "data.frame")
  expect_s3_class(result$effort_stratum_pe,  "data.frame")
  expect_s3_class(result$effort_stratum_bss, "data.frame")

  # Original stratum keys should not appear
  expect_null(result$catch_stratum)
  expect_null(result$effort_stratum)
})

test_that("tidy_fishery_estimates stratum tables use bare estimate_type column names", {
  mock_list <- create_mock_estimates_list_with_stratum()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  # PE stratum catch should have bare estimate_type names (no PE_ prefix)
  pe_cols <- names(result$catch_stratum_pe)
  expect_false(any(grepl("^PE_", pe_cols)))
  expect_false(any(grepl("^BSS_", pe_cols)))
  expect_true(any(grepl("catch_estimate_mean|number_observations", pe_cols)))

  # BSS stratum catch likewise
  bss_cols <- names(result$catch_stratum_bss)
  expect_false(any(grepl("^PE_", bss_cols)))
  expect_false(any(grepl("^BSS_", bss_cols)))
})

test_that("tidy_fishery_estimates stratum catch has catch_group; effort does not", {
  mock_list <- create_mock_estimates_list_with_stratum()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  expect_true("catch_group" %in% names(result$catch_stratum_pe))
  expect_true("catch_group" %in% names(result$catch_stratum_bss))
  expect_false("catch_group" %in% names(result$effort_stratum_pe))
  expect_false("catch_group" %in% names(result$effort_stratum_bss))
})

test_that("tidy_fishery_estimates stratum tables include stratum-specific columns", {
  mock_list <- create_mock_estimates_list_with_stratum()
  result <- suppressMessages(tidy_fishery_estimates(mock_list))

  stratum_cols <- c("angler_type_name", "section_num", "day_type", "period_timestep")
  for (col in stratum_cols) {
    expect_true(col %in% names(result$catch_stratum_pe),
                info = paste("Missing stratum column:", col, "in catch_stratum_pe"))
    expect_true(col %in% names(result$catch_stratum_bss),
                info = paste("Missing stratum column:", col, "in catch_stratum_bss"))
  }
})

# Basic Functionality Tests (legacy single-df path) ----

test_that("tidy_fishery_estimates returns correct structure", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  expect_type(result, "list")
  expect_named(result, c("catch", "effort"))
  expect_s3_class(result$catch, "data.frame")
  expect_s3_class(result$effort, "data.frame")
})
test_that("tidy_fishery_estimates separates catch and effort", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Catch table should have catch_group column with species names
  expect_true("catch_group" %in% names(result$catch))
  catch_groups <- unique(result$catch$catch_group)
  expect_false(any(catch_groups == "effort"))

  # Effort table should not have catch_group column
  expect_false("catch_group" %in% names(result$effort))
})

test_that("tidy_fishery_estimates pivots data wider correctly", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Check that estimate types became column names with model prefix
  catch_cols <- names(result$catch)
  expect_true(any(grepl("^PE_", catch_cols)))
  expect_true(any(grepl("^BSS_", catch_cols)))

  effort_cols <- names(result$effort)
  expect_true(any(grepl("^PE_", effort_cols)))
  expect_true(any(grepl("^BSS_", effort_cols)))
})

test_that("tidy_fishery_estimates creates prefixed estimate columns", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Should have PE_ and BSS_ prefixed columns
  expected_patterns <- c("PE_estimate_sum", "BSS_estimate_sum",
                         "PE_mean", "BSS_mean")

  catch_cols <- names(result$catch)
  for (pattern in expected_patterns) {
    expect_true(any(grepl(pattern, catch_cols)),
                info = paste("Missing pattern:", pattern))
  }
})

test_that("tidy_fishery_estimates reduces row count appropriately", {
  test_data <- create_mock_estimates_long()

  # Original data has multiple rows per catch_group (different estimate_types)
  n_catch_long <- sum(test_data$estimate_category == "catch")
  n_effort_long <- sum(test_data$estimate_category == "effort")

  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Wide format should have fewer rows (one per unique catch_group)
  expect_lt(nrow(result$catch), n_catch_long)
  expect_lt(nrow(result$effort), n_effort_long)

  # Should have one row per catch_group after collapsing
  n_unique_catch_groups <- length(unique(test_data$catch_group[test_data$estimate_category == "catch"]))
  expect_equal(nrow(result$catch), n_unique_catch_groups)
})

# Classification Tests ----

test_that("tidy_fishery_estimates classifies estimate_category correctly", {
  test_data <- create_mock_estimates_long()

  messages <- capture_messages(tidy_fishery_estimates(test_data))
  messages_text <- paste(messages, collapse = "\n")

  # Should report classification counts
  expect_match(messages_text, "Classified [0-9]+ catch records")
  expect_match(messages_text, "and [0-9]+ effort records")
})

test_that("tidy_fishery_estimates handles unrecognized estimate_category", {
  test_data <- create_mock_estimates_long()

  # Add some records with unrecognized category
  bad_rows <- test_data[1:2, ]
  bad_rows$estimate_category <- "unknown_category"
  test_data_with_bad <- rbind(test_data, bad_rows)

  expect_message(
    result <- tidy_fishery_estimates(test_data_with_bad),
    "WARNING: [0-9]+ rows with unrecognized estimate_category"
  )

  expect_message(
    result <- tidy_fishery_estimates(test_data_with_bad),
    "These records will be excluded from output"
  )
})

test_that("tidy_fishery_estimates recognizes C_sum and E_sum categories", {
  test_data <- create_mock_estimates_long()

  # Change some categories to C_sum and E_sum format
  test_data$estimate_category[test_data$estimate_category == "catch"] <- "C_sum"
  test_data$estimate_category[test_data$estimate_category == "effort"] <- "E_sum"

  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Should still work and classify correctly
  expect_s3_class(result$catch, "data.frame")
  expect_s3_class(result$effort, "data.frame")
  expect_gt(nrow(result$catch), 0)
  expect_gt(nrow(result$effort), 0)
})

# Model Row Collapsing Tests ----

test_that("tidy_fishery_estimates collapses PE and BSS rows", {
  test_data <- create_mock_estimates_long()

  messages <- capture_messages(tidy_fishery_estimates(test_data))
  messages_text <- paste(messages, collapse = "\n")

  # Should mention collapsing
  expect_match(messages_text, "Collapsing separate PE and BSS rows")
  expect_match(messages_text, "rows collapsed to")
})

test_that("collapsed rows contain both PE and BSS estimates", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Each row should have both PE_ and BSS_ columns populated
  pe_cols <- grep("^PE_", names(result$catch), value = TRUE)
  bss_cols <- grep("^BSS_", names(result$catch), value = TRUE)

  expect_gt(length(pe_cols), 0)
  expect_gt(length(bss_cols), 0)

  # Check that at least some PE and BSS values are non-NA
  expect_true(any(!is.na(result$catch[[pe_cols[1]]])))
  expect_true(any(!is.na(result$catch[[bss_cols[1]]])))
})

# Column Ordering Tests ----

test_that("tidy_fishery_estimates orders columns logically", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  catch_cols <- names(result$catch)

  # ID columns should come first
  id_pattern_position <- which(grepl("^analysis_id|^project_id|^fishery_name", catch_cols))
  expect_true(all(id_pattern_position <= 5))

  # Estimate columns (with PE_ or BSS_ prefix) should come last
  estimate_position <- which(grepl("^(PE_|BSS_)", catch_cols))
  other_position <- which(!grepl("^(PE_|BSS_)", catch_cols))

  if (length(estimate_position) > 0 && length(other_position) > 0) {
    expect_true(min(estimate_position) > max(other_position))
  }
})

test_that("tidy_fishery_estimates includes catch_group in catch table only", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Catch should have catch_group
  expect_true("catch_group" %in% names(result$catch))

  # Effort should not have catch_group
  expect_false("catch_group" %in% names(result$effort))
})

# Edge Cases ----

test_that("tidy_fishery_estimates handles only catch data", {
  test_data <- create_mock_estimates_long() |>
    dplyr::filter(estimate_category == "catch")

  expect_message(
    result <- tidy_fishery_estimates(test_data),
    "No effort data to pivot"
  )

  expect_s3_class(result$catch, "data.frame")
  expect_null(result$effort)
})

test_that("tidy_fishery_estimates handles only effort data", {
  test_data <- create_mock_estimates_long() |>
    dplyr::filter(estimate_category == "effort")

  expect_message(
    result <- tidy_fishery_estimates(test_data),
    "No catch data to pivot"
  )

  expect_null(result$catch)
  expect_s3_class(result$effort, "data.frame")
})

test_that("tidy_fishery_estimates handles single catch group", {
  test_data <- create_mock_estimates_long() |>
    dplyr::filter(catch_group %in% c("Chinook_Adult_AD_Kept", "effort"))

  result <- suppressMessages(tidy_fishery_estimates(test_data))

  expect_equal(nrow(result$catch), 1)
  expect_s3_class(result$effort, "data.frame")
})

test_that("tidy_fishery_estimates handles duplicate estimate values", {
  test_data <- create_mock_estimates_long()

  # Duplicate some rows to create multiple values for same estimate_type
  dup_rows <- test_data[1:2, ]
  test_data_with_dups <- rbind(test_data, dup_rows)

  expect_message(
    result <- tidy_fishery_estimates(test_data_with_dups),
    "WARNING: Multiple values found for same estimate_type"
  )

  # Should still return valid result (using first value)
  expect_s3_class(result$catch, "data.frame")
})

test_that("tidy_fishery_estimates handles NA in estimate values", {
  test_data <- create_mock_estimates_long()
  test_data$estimate_value[c(1, 5, 10)] <- NA

  result <- suppressMessages(tidy_fishery_estimates(test_data))

  expect_s3_class(result$catch, "data.frame")
  expect_s3_class(result$effort, "data.frame")
})

# Data Integrity Tests ----

test_that("tidy_fishery_estimates preserves metadata columns", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Key metadata should be preserved
  metadata_cols <- c("analysis_id", "fishery_name", "project_name",
                     "min_event_date", "max_event_date")

  expect_true(all(metadata_cols %in% names(result$catch)))
})

test_that("tidy_fishery_estimates does not create .x or .y columns", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # Should not have any .x or .y suffixed columns
  duplicate_pattern <- "\\.(x|y)$"
  expect_false(any(grepl(duplicate_pattern, names(result$catch))))
  expect_false(any(grepl(duplicate_pattern, names(result$effort))))
})

test_that("tidy_fishery_estimates does not create estimate_class column in output", {
  test_data <- create_mock_estimates_long()
  result <- suppressMessages(tidy_fishery_estimates(test_data))

  # estimate_class is internal only, should not appear in output
  expect_false("estimate_class" %in% names(result$catch))
  expect_false("estimate_class" %in% names(result$effort))
})

# Message Output Tests ----

test_that("tidy_fishery_estimates provides progress messages", {
  test_data <- create_mock_estimates_long()

  messages <- capture_messages(tidy_fishery_estimates(test_data))
  messages_text <- paste(messages, collapse = "\n")

  expect_match(messages_text, "Converting [0-9]+ rows to tidy format")
  expect_match(messages_text, "Classified [0-9]+ catch records")
  expect_match(messages_text, "Pivoting catch data to wide format")
  expect_match(messages_text, "Pivoting effort data to wide format")
  expect_match(messages_text, "Successfully created tidy estimates")
})

test_that("tidy_fishery_estimates list-path provides progress messages", {
  mock_list <- create_mock_estimates_list()

  messages <- capture_messages(tidy_fishery_estimates(mock_list))
  messages_text <- paste(messages, collapse = "\n")

  expect_match(messages_text, "Input detected as named list")
  expect_match(messages_text, "Pivoting 'catch_total'")
  expect_match(messages_text, "Pivoting 'effort_total'")
  expect_match(messages_text, "Successfully created tidy estimates")
})

test_that("tidy_fishery_estimates list-path with stratum shows split model messages", {
  mock_list <- create_mock_estimates_list_with_stratum()

  messages <- capture_messages(tidy_fishery_estimates(mock_list))
  messages_text <- paste(messages, collapse = "\n")

  expect_match(messages_text, "Pivoting 'catch_stratum_pe'")
  expect_match(messages_text, "Pivoting 'catch_stratum_bss'")
  expect_match(messages_text, "Pivoting 'effort_stratum_pe'")
  expect_match(messages_text, "Pivoting 'effort_stratum_bss'")
})

# Integration with get_fishery_estimates ----

test_that("tidy_fishery_estimates works with get_fishery_estimates output", {
  skip_if_offline <- function() {
    con_available <- tryCatch({
      con <- creelutils::establish_db_con()
      valid <- DBI::dbIsValid(con)
      if (valid) DBI::dbDisconnect(con)
      valid
    }, error = function(e) FALSE)

    if (!con_available) skip("Database connection not available")
  }

  skip_if_offline()

  # Get real data as a list
  raw_data <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      temporal_agg = "total"
    )
  )

  skip_if(is.null(raw_data), "No data available for test")

  # Pass the full list — should return matching named list
  result <- suppressMessages(tidy_fishery_estimates(raw_data))

  expect_type(result, "list")
  expect_true("catch_total" %in% names(result))
  expect_true("effort_total" %in% names(result))
  if (!is.null(result$catch_total)) {
    expect_s3_class(result$catch_total, "data.frame")
  }
  if (!is.null(result$effort_total)) {
    expect_s3_class(result$effort_total, "data.frame")
  }
})

# Performance Tests ----

test_that("tidy_fishery_estimates handles large datasets", {
  # Create larger test dataset
  large_data <- do.call(rbind, replicate(100, create_mock_estimates_long(), simplify = FALSE))
  large_data$analysis_id <- paste0("uuid-", rep(1:100, each = 24))

  expect_silent(
    result <- suppressMessages(tidy_fishery_estimates(large_data))
  )

  expect_s3_class(result$catch, "data.frame")
  expect_s3_class(result$effort, "data.frame")
})
