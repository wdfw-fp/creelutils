# Helper functions for get_fishery_estimates tests

# Check if we can connect to the database
can_connect_to_db <- function() {
  tryCatch({
    con <- creelutils::establish_db_con()
    valid <- DBI::dbIsValid(con)
    if (valid) DBI::dbDisconnect(con)
    valid
  }, error = function(e) FALSE)
}

# Get a valid test fishery name from the database
get_test_fishery <- function() {
  if (!can_connect_to_db()) return(NULL)

  tryCatch({
    con <- creelutils::establish_db_con()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Get any fishery with data
    fisheries <- creelutils::fetch_db_table(con, "creel", "model_analysis_lut")

    if (nrow(fisheries) > 0) {
      # Extract fishery name from first analysis
      fishery_name <- stringr::str_extract(fisheries$analysis_name[1], "^[^_]+")
      return(fishery_name)
    }

    NULL
  }, error = function(e) NULL)
}
