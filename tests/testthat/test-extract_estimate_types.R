# Tests for extract_estimate_types()
# Add to: tests/testthat/test-tidy_fishery_estimates.R

# Setup for extract_estimate_types tests ----

create_mock_tidy_catch <- function() {
  tibble::tibble(
    analysis_id = rep("uuid-001", 2),
    fishery_name = rep("Nisqually", 2),
    catch_group = c("Chinook_Adult_AD_Kept", "Steelhead_Adult_UM_Released"),
    PE_estimate_sum = c(100, 50),
    PE_mean = c(95, 48),
    PE_standard_error_mean = c(5, 3),
    BSS_estimate_sum = c(105, 52),
    BSS_mean = c(100, 50),
    BSS_quantile_lower_2_5 = c(90, 45),
    BSS_quantile_upper_97_5 = c(110, 55),
    BSS_n_eff = c(1000, 1000),
    BSS_R_hat = c(1.01, 1.00)
  )
}

# extract_estimate_types Tests ----

test_that("extract_estimate_types requires a dataframe", {
  expect_message(
    result <- extract_estimate_types("not a dataframe"),
    "ERROR: 'tidy_table' must be a dataframe"
  )
  expect_null(result)
})

test_that("extract_estimate_types returns data with default patterns", {
  test_data <- create_mock_tidy_catch()

  result <- suppressMessages(extract_estimate_types(test_data))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(test_data))
})

test_that("extract_estimate_types filters to matching patterns", {
  test_data <- create_mock_tidy_catch()

  # Extract only PE estimates
  result <- suppressMessages(extract_estimate_types(test_data, "^PE_"))

  result_cols <- names(result)
  pe_cols <- grep("^PE_", result_cols, value = TRUE)
  bss_cols <- grep("^BSS_", result_cols, value = TRUE)

  expect_gt(length(pe_cols), 0)
  expect_equal(length(bss_cols), 0)
})

test_that("extract_estimate_types keeps metadata columns", {
  test_data <- create_mock_tidy_catch()

  result <- suppressMessages(extract_estimate_types(test_data, "^BSS_mean$"))

  # Should still have metadata
  expect_true("analysis_id" %in% names(result))
  expect_true("fishery_name" %in% names(result))
  expect_true("catch_group" %in% names(result))
})

test_that("extract_estimate_types handles multiple patterns", {
  test_data <- create_mock_tidy_catch()

  patterns <- c("estimate_sum$", "mean$")
  result <- suppressMessages(extract_estimate_types(test_data, patterns))

  result_cols <- names(result)

  # Should have columns matching both patterns
  expect_true(any(grepl("estimate_sum$", result_cols)))
  expect_true(any(grepl("mean$", result_cols)))

  # Should not have other estimate columns
  expect_false(any(grepl("standard_error_mean$", result_cols)))
  expect_false(any(grepl("n_eff$", result_cols)))
})

test_that("extract_estimate_types handles no matches gracefully", {
  test_data <- create_mock_tidy_catch()

  expect_message(
    result <- extract_estimate_types(test_data, "nonexistent_pattern"),
    "WARNING: No columns matched the provided patterns"
  )

  # Should return original data when no matches
  expect_equal(names(result), names(test_data))
})

test_that("extract_estimate_types reports number of columns extracted", {
  test_data <- create_mock_tidy_catch()

  expect_message(
    result <- extract_estimate_types(test_data, "^PE_"),
    "Extracting [0-9]+ estimate column\\(s\\)"
  )
})

test_that("extract_estimate_types handles regex patterns correctly", {
  test_data <- create_mock_tidy_catch()

  # Extract confidence intervals
  result <- suppressMessages(
    extract_estimate_types(test_data, "quantile_(lower|upper)")
  )

  result_cols <- names(result)
  expect_true(any(grepl("quantile_lower", result_cols)))
  expect_true(any(grepl("quantile_upper", result_cols)))
  expect_false(any(grepl("^PE_estimate_sum", result_cols)))
})
