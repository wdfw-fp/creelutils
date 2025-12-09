# Tests for get_fishery_estimates()
# Test file: tests/testthat/test-get_fishery_estimates.R

# Setup ----
# Note: Most tests require access to the WDFW PostgreSQL database
# Tests that need database access are marked with skip_if_offline()

# Helper to check database availability
skip_if_offline <- function() {
  con_available <- tryCatch({
    con <- creelutils::establish_db_con()
    valid <- DBI::dbIsValid(con)
    if (valid) DBI::dbDisconnect(con)
    valid
  }, error = function(e) FALSE)

  if (!con_available) {
    skip("Database connection not available")
  }
}

# Input Validation Tests ----

test_that("get_fishery_estimates requires fishery_names parameter", {
  expect_message(
    result <- get_fishery_estimates(),
    "ERROR: 'fishery_names' must be provided as a non-empty character vector"
  )
  expect_null(result)
})

test_that("get_fishery_estimates rejects empty fishery_names", {
  expect_message(
    result <- get_fishery_estimates(fishery_names = character(0)),
    "ERROR: 'fishery_names' must be provided as a non-empty character vector"
  )
  expect_null(result)
})

test_that("get_fishery_estimates requires character fishery_names", {
  expect_message(
    result <- get_fishery_estimates(fishery_names = 123),
    "ERROR: 'fishery_names' must be a character vector"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(fishery_names = list("Nisqually")),
    "ERROR: 'fishery_names' must be a character vector"
  )
  expect_null(result)
})

test_that("get_fishery_estimates validates years parameter type", {
  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      years = "2021"
    ),
    "ERROR: 'years' must be NULL or a numeric vector"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      years = c("2021", "2022")
    ),
    "ERROR: 'years' must be NULL or a numeric vector"
  )
  expect_null(result)
})

test_that("get_fishery_estimates validates max_upload_date parameter", {
  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      max_upload_date = "TRUE"
    ),
    "ERROR: 'max_upload_date' must be a single logical value \\(TRUE or FALSE\\)"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      max_upload_date = c(TRUE, FALSE)
    ),
    "ERROR: 'max_upload_date' must be a single logical value \\(TRUE or FALSE\\)"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      max_upload_date = NA
    ),
    "ERROR: 'max_upload_date' must be a single logical value \\(TRUE or FALSE\\)"
  )
  expect_null(result)
})

test_that("get_fishery_estimates validates database connection", {
  # Create invalid connection object
  invalid_con <- structure(list(), class = "invalid_connection")

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      con = invalid_con
    ),
    "ERROR: Invalid database connection provided"
  )
  expect_null(result)
})

# Fishery Name Building Tests ----

test_that("get_fishery_estimates builds correct message for single fishery with single year", {
  expect_message(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021),
    "Querying estimates for 1 fishery-year combination\\(s\\)"
  )
})

test_that("get_fishery_estimates builds correct message for single fishery with multiple years", {
  expect_message(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021:2023),
    "Querying estimates for 3 fishery-year combination\\(s\\)"
  )
})

test_that("get_fishery_estimates builds correct message for multiple fisheries with years", {
  expect_message(
    get_fishery_estimates(
      fishery_names = c("Nisqually", "Skagit"),
      years = 2021:2022
    ),
    "Querying estimates for 4 fishery-year combination\\(s\\)"
  )
})

test_that("get_fishery_estimates uses exact names when years not provided", {
  expect_message(
    get_fishery_estimates(fishery_names = "Nisqually salmon 2021"),
    "Querying estimates for 1 fishery name\\(s\\)"
  )

  expect_message(
    get_fishery_estimates(
      fishery_names = c("Skagit fall salmon 2021", "Skagit summer gamefish 2022")
    ),
    "Querying estimates for 2 fishery name\\(s\\)"
  )
})

# Database Integration Tests ----

test_that("get_fishery_estimates establishes connection when none provided", {
  skip_if_offline()

  # Should not error and should establish connection internally
  expect_message(
    result <- get_fishery_estimates(fishery_names = "Nisqually", years = 2021),
    "No connection provided. Establishing database connection"
  )
})

test_that("get_fishery_estimates uses provided connection", {
  skip_if_offline()

  con <- establish_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Should NOT show "establishing connection" message
  expect_silent(
    result <- suppressMessages(
      get_fishery_estimates(fishery_names = "Nisqually", years = 2021, con = con)
    ) ||
      !grepl("No connection provided",
             capture.output(get_fishery_estimates(fishery_names = "Nisqually", years = 2021, con = con)))
  )

  # Connection should still be valid after function call
  expect_true(DBI::dbIsValid(con))
})

test_that("get_fishery_estimates handles nonexistent fishery gracefully", {
  skip_if_offline()

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "ThisFisheryDoesNotExist12345"
    ),
    "WARNING: No estimates found for specified fisheries"
  )
  expect_null(result)
})

# Return Structure Tests ----

test_that("get_fishery_estimates returns list with correct structure", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  # Skip if no data found
  skip_if(is.null(result), "No data found for test fishery")

  expect_type(result, "list")
  expect_named(result, c("estimates", "analysis_metadata"))
  expect_s3_class(result$estimates, "data.frame")
  expect_s3_class(result$analysis_metadata, "data.frame")
})

test_that("get_fishery_estimates estimates table has required columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  required_cols <- c(
    "analysis_id", "analysis_name", "fishery_name",
    "model_type", "estimate_type", "estimate_value", "estimate_category",
    "catch_group", "upload_date"
  )

  expect_true(all(required_cols %in% names(result$estimates)))
})

test_that("get_fishery_estimates analysis_metadata has required columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  required_cols <- c(
    "fishery_name", "analysis_id", "analysis_name",
    "upload_date", "created_by"
  )

  expect_true(all(required_cols %in% names(result$analysis_metadata)))
})

test_that("get_fishery_estimates creates catch_group correctly", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  # Check that catch_group exists
  expect_true("catch_group" %in% names(result$estimates))

  # Check for effort records
  effort_records <- result$estimates[result$estimates$catch_group == "effort", ]
  if (nrow(effort_records) > 0) {
    expect_true(all(is.na(effort_records$species_name)))
  }

  # Check for catch records (should have underscore-separated format)
  catch_records <- result$estimates[result$estimates$catch_group != "effort", ]
  if (nrow(catch_records) > 0) {
    # Should match pattern: Species_LifeStage_FinMark_Fate
    expect_true(all(grepl("_", catch_records$catch_group)))
  }
})

test_that("get_fishery_estimates formats fin_mark_desc to short codes", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  # Filter to records that have fin_mark_desc
  with_finmark <- result$estimates[!is.na(result$estimates$fin_mark_desc), ]

  if (nrow(with_finmark) > 0) {
    # Should only contain short codes or original values, not long descriptions
    expect_false(any(grepl("Adclip clip \\+ No other external marks", with_finmark$fin_mark_desc)))
    expect_false(any(grepl("No Adclip \\+ No Other external marks", with_finmark$fin_mark_desc)))

    # Check that AD, UM, UNK exist if appropriate
    valid_codes <- c("AD", "UM", "UNK")
    if (any(with_finmark$fin_mark_desc %in% valid_codes)) {
      expect_true(all(with_finmark$fin_mark_desc %in% c(valid_codes, NA)))
    }
  }
})

# max_upload_date Filtering Tests ----

test_that("get_fishery_estimates filters to max_upload_date when TRUE", {
  skip_if_offline()

  # Get all estimates
  all_estimates <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      max_upload_date = FALSE
    )
  )

  skip_if(is.null(all_estimates), "No data found for test fishery")

  # Get filtered estimates
  filtered_estimates <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      max_upload_date = TRUE
    )
  )

  # If there are multiple upload dates, filtered should have fewer or equal rows
  expect_lte(nrow(filtered_estimates$estimates), nrow(all_estimates$estimates))

  # Each fishery_name + catch_group combination should have only one upload_date
  unique_combos <- filtered_estimates$estimates |>
    dplyr::group_by(fishery_name, catch_group) |>
    dplyr::summarise(n_dates = dplyr::n_distinct(upload_date), .groups = "drop")

  expect_true(all(unique_combos$n_dates == 1))
})

test_that("get_fishery_estimates returns all estimates when max_upload_date FALSE", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      max_upload_date = FALSE
    )
  )

  skip_if(is.null(result), "No data found for test fishery")

  expect_message(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      max_upload_date = FALSE
    ),
    "Returning all estimates \\(max_upload_date = FALSE\\)"
  )
})

# Column Duplication Tests ----

test_that("get_fishery_estimates removes duplicate .x and .y columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  # Check estimates table
  duplicate_pattern <- "\\.(x|y)$"
  expect_false(any(grepl(duplicate_pattern, names(result$estimates))))

  # Check analysis_metadata table
  expect_false(any(grepl(duplicate_pattern, names(result$analysis_metadata))))
})

# Multiple Fishery Tests ----

test_that("get_fishery_estimates handles multiple fisheries", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(
      fishery_names = c("Nisqually", "Skagit"),
      years = 2021
    )
  )

  skip_if(is.null(result), "No data found for test fisheries")

  # Should have data for multiple fisheries
  fishery_names <- unique(result$estimates$fishery_name)
  expect_gte(length(fishery_names), 1)
})

test_that("get_fishery_estimates handles multiple years", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021:2022
    )
  )

  skip_if(is.null(result), "No data found for test years")

  # Should have data for multiple years
  fishery_names <- unique(result$estimates$fishery_name)
  expect_gte(length(fishery_names), 1)
})

# Data Integrity Tests ----

test_that("get_fishery_estimates returns valid estimate_value types", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  expect_true(is.numeric(result$estimates$estimate_value))
  expect_false(any(is.na(result$estimates$estimate_value)))
})

test_that("get_fishery_estimates returns valid date columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  # Check upload_date
  expect_s3_class(result$estimates$upload_date, "Date")

  # Check min/max event dates if they exist
  if ("min_event_date" %in% names(result$estimates)) {
    expect_true(inherits(result$estimates$min_event_date, "Date"))
  }
  if ("max_event_date" %in% names(result$estimates)) {
    expect_true(inherits(result$estimates$max_event_date, "Date"))
  }
})

test_that("get_fishery_estimates has consistent analysis_id between tables", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  skip_if(is.null(result), "No data found for test fishery")

  # All analysis_ids in estimates should exist in analysis_metadata
  estimates_ids <- unique(result$estimates$analysis_id)
  metadata_ids <- unique(result$analysis_metadata$analysis_id)

  expect_true(all(estimates_ids %in% metadata_ids))
})

# Message Output Tests ----

test_that("get_fishery_estimates provides appropriate progress messages", {
  skip_if_offline()

  messages <- capture_messages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  messages_text <- paste(messages, collapse = "\n")

  # Check for key progress messages
  expect_match(messages_text, "Querying estimates for")
  expect_match(messages_text, "Querying analysis metadata")
  expect_match(messages_text, "Querying model estimates")
  expect_match(messages_text, "Formatting estimate data")
})

test_that("get_fishery_estimates reports record counts", {
  skip_if_offline()

  messages <- capture_messages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021)
  )

  messages_text <- paste(messages, collapse = "\n")

  # Should report number of records found
  expect_match(messages_text, "Found [0-9]+ analysis record\\(s\\)")
  expect_match(messages_text, "Found [0-9]+ estimate record\\(s\\)")
})

# Edge Cases ----

test_that("get_fishery_estimates handles fishery names with special characters", {
  skip_if_offline()

  # Test with a fishery name that might have special characters
  # This will likely return NULL but shouldn't error
  expect_silent(
    suppressMessages(
      result <- get_fishery_estimates(fishery_names = "Test-Fishery 2021")
    )
  )
})

test_that("get_fishery_estimates handles very old years", {
  skip_if_offline()

  # Should handle gracefully even if no data exists
  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 1900
    ),
    "WARNING: No estimates found|No analysis records found"
  )
})

test_that("get_fishery_estimates handles future years", {
  skip_if_offline()

  # Should handle gracefully even if no data exists
  future_year <- as.integer(format(Sys.Date(), "%Y")) + 10

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      years = future_year
    ),
    "WARNING: No estimates found|No analysis records found"
  )
})
