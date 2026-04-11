# Tests for get_fishery_estimates()
# Test file: tests/testthat/test-get_fishery_estimates.R

# Setup ----
# Note: Most tests require access to the WDFW PostgreSQL database
# Tests that need database access are marked with skip_if_offline()

# Helper to check database availability
skip_if_offline <- function() {
  con_available <- tryCatch({
    con <- creelutils::connect_creel_db()
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

test_that("get_fishery_estimates validates query_timeout parameter", {
  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      query_timeout = "60"
    ),
    "ERROR: 'query_timeout' must be NULL or a single positive number"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      query_timeout = -5
    ),
    "ERROR: 'query_timeout' must be NULL or a single positive number"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      query_timeout = 0
    ),
    "ERROR: 'query_timeout' must be NULL or a single positive number"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      query_timeout = c(30, 60)
    ),
    "ERROR: 'query_timeout' must be NULL or a single positive number"
  )
  expect_null(result)

  # NULL and valid positive values should pass validation (will fail at DB step)
  expect_message(
    get_fishery_estimates(fishery_names = "Nisqually", query_timeout = NULL),
    "Querying estimates"
  )
  expect_message(
    get_fishery_estimates(fishery_names = "Nisqually", query_timeout = 30),
    "Querying estimates"
  )
})

test_that("get_fishery_estimates validates temporal_agg parameter", {
  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      temporal_agg = "both"
    ),
    "ERROR: 'temporal_agg' must be one of: 'total', 'stratum', or 'all'"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      temporal_agg = c("total", "stratum")
    ),
    "ERROR: 'temporal_agg' must be one of: 'total', 'stratum', or 'all'"
  )
  expect_null(result)

  expect_message(
    result <- get_fishery_estimates(
      fishery_names = "Nisqually",
      temporal_agg = 1
    ),
    "ERROR: 'temporal_agg' must be one of: 'total', 'stratum', or 'all'"
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
  # interaction() pairs element-wise: c("A","B") x c(2021,2022) = 2 combinations
  expect_message(
    get_fishery_estimates(
      fishery_names = c("Nisqually", "Skagit"),
      years = 2021:2022
    ),
    "Querying estimates for 2 fishery-year combination\\(s\\)"
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

  con <- connect_creel_db()
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

test_that("get_fishery_estimates returns list with correct structure (temporal_agg = 'all')", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "all")
  )

  skip_if(is.null(result), "No data found for test fishery")

  expect_type(result, "list")
  expected_names <- c("analysis_metadata", "catch_total", "effort_total",
                      "catch_stratum", "effort_stratum", "catchrate_stratum")
  expect_true(all(expected_names %in% names(result)))
  expect_s3_class(result$analysis_metadata, "data.frame")
})

test_that("get_fishery_estimates returns only total tables when temporal_agg = 'total'", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")

  expect_true(all(c("catch_total", "effort_total", "analysis_metadata") %in% names(result)))
  expect_false("catch_stratum" %in% names(result))
  expect_false("effort_stratum" %in% names(result))
  expect_false("estimates" %in% names(result))
})

test_that("get_fishery_estimates returns only stratum tables when temporal_agg = 'stratum'", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "stratum")
  )

  skip_if(is.null(result), "No data found for test fishery")

  expect_true(all(c("catch_stratum", "effort_stratum", "catchrate_stratum", "analysis_metadata") %in% names(result)))
  expect_false("catch_total" %in% names(result))
  expect_false("effort_total" %in% names(result))
  expect_false("estimates" %in% names(result))
})

test_that("get_fishery_estimates catch_total table has required columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$catch_total) || nrow(result$catch_total) == 0, "No catch_total data")

  required_cols <- c(
    "analysis_id", "analysis_name", "fishery_name",
    "model_type", "estimate_type", "estimate_value", "estimate_category",
    "catch_group", "upload_date"
  )

  expect_true(all(required_cols %in% names(result$catch_total)))
})

test_that("get_fishery_estimates catch_stratum table has stratum-specific columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "stratum")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$catch_stratum) || nrow(result$catch_stratum) == 0, "No catch_stratum data")

  # Stratum-specific columns
  stratum_cols <- c("angler_type_name", "section_num", "day_type", "period_timestep")
  expect_true(any(stratum_cols %in% names(result$catch_stratum)))
})

test_that("get_fishery_estimates analysis_metadata has required columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")

  required_cols <- c(
    "fishery_name", "analysis_id", "analysis_name",
    "upload_date", "created_by"
  )

  expect_true(all(required_cols %in% names(result$analysis_metadata)))
})

test_that("get_fishery_estimates creates catch_group correctly in catch_total", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$catch_total) || nrow(result$catch_total) == 0, "No catch_total data")

  # All records in catch_total should be catch (not effort)
  expect_true("catch_group" %in% names(result$catch_total))
  expect_false(any(result$catch_total$catch_group == "effort"))

  # catch_group should have underscore-separated format
  expect_true(all(grepl("_", result$catch_total$catch_group)))
})

test_that("get_fishery_estimates effort tables contain only effort records", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$effort_total) || nrow(result$effort_total) == 0, "No effort_total data")

  expect_true(all(result$effort_total$estimate_category %in% c("effort", "E_sum")))
})

test_that("get_fishery_estimates formats fin_mark to short codes", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$catch_total) || nrow(result$catch_total) == 0, "No catch_total data")

  with_finmark <- result$catch_total[!is.na(result$catch_total$fin_mark), ]

  if (nrow(with_finmark) > 0) {
    expect_false(any(grepl("Adclip clip \\+ No other external marks", with_finmark$fin_mark)))
    expect_false(any(grepl("No Adclip \\+ No Other external marks", with_finmark$fin_mark)))

    valid_codes <- c("AD", "UM", "UNK")
    if (any(with_finmark$fin_mark %in% valid_codes)) {
      expect_true(all(with_finmark$fin_mark %in% c(valid_codes, NA)))
    }
  }
})

# max_upload_date Filtering Tests ----

test_that("get_fishery_estimates filters to max_upload_date when TRUE", {
  skip_if_offline()

  all_estimates <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      temporal_agg = "total",
      max_upload_date = FALSE
    )
  )

  skip_if(is.null(all_estimates), "No data found for test fishery")
  skip_if(is.null(all_estimates$catch_total), "No catch_total data")

  filtered_estimates <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      temporal_agg = "total",
      max_upload_date = TRUE
    )
  )

  expect_lte(nrow(filtered_estimates$catch_total), nrow(all_estimates$catch_total))

  unique_combos <- filtered_estimates$catch_total |>
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
      temporal_agg = "total",
      max_upload_date = FALSE
    )
  )

  skip_if(is.null(result), "No data found for test fishery")

  expect_message(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021,
      temporal_agg = "total",
      max_upload_date = FALSE
    ),
    "Returning all available estimates \\(max_upload_date = FALSE\\)"
  )
})

# Column Duplication Tests ----

test_that("get_fishery_estimates removes duplicate .x and .y columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "all")
  )

  skip_if(is.null(result), "No data found for test fishery")

  duplicate_pattern <- "\\.(x|y)$"
  for (tbl_name in c("catch_total", "effort_total", "catch_stratum", "effort_stratum", "catchrate_stratum")) {
    if (!is.null(result[[tbl_name]])) {
      expect_false(
        any(grepl(duplicate_pattern, names(result[[tbl_name]]))),
        info = paste("Duplicate columns found in", tbl_name)
      )
    }
  }
  expect_false(any(grepl(duplicate_pattern, names(result$analysis_metadata))))
})

# Multiple Fishery Tests ----

test_that("get_fishery_estimates handles multiple fisheries", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(
      fishery_names = c("Nisqually", "Skagit"),
      years = 2021,
      temporal_agg = "total"
    )
  )

  skip_if(is.null(result), "No data found for test fisheries")
  skip_if(is.null(result$catch_total), "No catch_total data")

  fishery_names <- unique(result$catch_total$fishery_name)
  expect_gte(length(fishery_names), 1)
})

test_that("get_fishery_estimates handles multiple years", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(
      fishery_names = "Nisqually",
      years = 2021:2022,
      temporal_agg = "total"
    )
  )

  skip_if(is.null(result), "No data found for test years")
  skip_if(is.null(result$catch_total), "No catch_total data")

  fishery_names <- unique(result$catch_total$fishery_name)
  expect_gte(length(fishery_names), 1)
})

# Data Integrity Tests ----

test_that("get_fishery_estimates returns valid estimate_value types", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$catch_total) || nrow(result$catch_total) == 0, "No catch_total data")

  expect_true(is.numeric(result$catch_total$estimate_value))
})

test_that("get_fishery_estimates returns valid date columns", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$catch_total) || nrow(result$catch_total) == 0, "No catch_total data")

  expect_s3_class(result$catch_total$upload_date, "Date")

  if ("min_event_date" %in% names(result$catch_total)) {
    expect_true(inherits(result$catch_total$min_event_date, "Date"))
  }
  if ("max_event_date" %in% names(result$catch_total)) {
    expect_true(inherits(result$catch_total$max_event_date, "Date"))
  }
})

test_that("get_fishery_estimates has consistent analysis_id between tables", {
  skip_if_offline()

  result <- suppressMessages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  skip_if(is.null(result), "No data found for test fishery")
  skip_if(is.null(result$catch_total) || nrow(result$catch_total) == 0, "No catch_total data")

  estimates_ids <- unique(result$catch_total$analysis_id)
  metadata_ids <- unique(result$analysis_metadata$analysis_id)

  expect_true(all(estimates_ids %in% metadata_ids))
})

# Message Output Tests ----

test_that("get_fishery_estimates provides appropriate progress messages", {
  skip_if_offline()

  messages <- capture_messages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  messages_text <- paste(messages, collapse = "\n")

  expect_match(messages_text, "Querying estimates for")
  expect_match(messages_text, "analysis metadata|analysis record")
  expect_match(messages_text, "total-level estimates|total-level estimate")
})

test_that("get_fishery_estimates reports record counts", {
  skip_if_offline()

  messages <- capture_messages(
    get_fishery_estimates(fishery_names = "Nisqually", years = 2021, temporal_agg = "total")
  )

  messages_text <- paste(messages, collapse = "\n")

  expect_match(messages_text, "analysis record|Found [0-9]+ analysis")
  expect_match(messages_text, "catch.*effort|estimate record")
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
