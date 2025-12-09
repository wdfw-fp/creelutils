# Tests for tidy_fishery_estimates()
# Test file: tests/testthat/test-tidy_fishery_estimates.R

# Setup test data ----

# Helper function to create mock long-format estimates data
create_mock_estimates_long <- function() {
  tibble::tibble(
    analysis_id = rep("uuid-001", 24),
    analysis_name = rep("Nisqually_2021_PE_final", 24),
    project_id = rep("proj-001", 24),
    project_name = rep("Test Project", 24),
    fishery_name = rep("Nisqually", 24),

    # Catch records (12 rows: 2 catch groups × 2 models × 3 estimate types)
    species_name = c(rep("Chinook", 6), rep("Steelhead", 6), rep(NA, 12)),
    life_stage_name = c(rep("Adult", 6), rep("Adult", 6), rep(NA, 12)),
    fin_mark_desc = c(rep("AD", 6), rep("UM", 6), rep(NA, 12)),
    fate_name = c(rep("Kept", 6), rep("Released", 6), rep(NA, 12)),
    catch_group = c(
      rep("Chinook_Adult_AD_Kept", 6),
      rep("Steelhead_Adult_UM_Released", 6),
      rep("effort", 12)
    ),

    # Model types and estimate types
    model_type = rep(c("PE", "PE", "PE", "BSS", "BSS", "BSS"), 4),
    estimate_type = rep(c("estimate_sum", "mean", "standard_error_mean"), 8),
    estimate_value = c(
      # Chinook PE
      100, 95, 5,
      # Chinook BSS
      105, 100, 6,
      # Steelhead PE
      50, 48, 3,
      # Steelhead BSS
      52, 50, 4,
      # Effort PE
      1000, 980, 20,
      # Effort BSS
      1050, 1000, 25,
      # More effort
      2000, 1950, 30,
      2100, 2050, 35
    ),

    estimate_category = c(
      rep("catch", 12),
      rep("effort", 12)
    ),

    period = rep(1, 24),
    timestep = rep("week", 24),
    min_event_date = rep(as.Date("2021-01-01"), 24),
    max_event_date = rep(as.Date("2021-12-31"), 24),
    data_grade = rep("Approved", 24),
    upload_date = rep(as.Date("2021-12-15"), 24),
    created_by = rep("test_user", 24)
  )
}

# Input Validation Tests ----

test_that("tidy_fishery_estimates requires a dataframe", {
  expect_message(
    result <- tidy_fishery_estimates(),
    "ERROR: 'estimates_df' must be a dataframe"
  )
  expect_null(result)

  expect_message(
    result <- tidy_fishery_estimates("not a dataframe"),
    "ERROR: 'estimates_df' must be a dataframe"
  )
  expect_null(result)

  expect_message(
    result <- tidy_fishery_estimates(list(a = 1, b = 2)),
    "ERROR: 'estimates_df' must be a dataframe"
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

# Basic Functionality Tests ----

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

  # Get real data
  raw_data <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(raw_data), "No data available for test")

  # Should be able to tidy it
  result <- suppressMessages(tidy_fishery_estimates(raw_data$estimates))

  expect_type(result, "list")
  expect_s3_class(result$catch, "data.frame")
  expect_s3_class(result$effort, "data.frame")
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
