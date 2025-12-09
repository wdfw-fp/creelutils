# Helper functions for tidy_fishery_estimates tests

# Skip function for offline tests
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
