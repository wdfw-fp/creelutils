#' establish_db_con
#'
#' Establishes a connection to the WDFW PostgreSQL database
#' @param max_attempts .
#' @param delay_seconds .
#'
#' @return con
#' @export

establish_db_con<- function(max_attempts = 5, delay_seconds = 3) {

  #load config file
  config <- tryCatch({
    yaml::read_yaml(here::here("config.yml"))
  }, error = function(e) {
    NULL
  })

  sys_user <- unname(Sys.info()["user"])

  if (!is.null(config) && "users" %in% names(config) && sys_user %in% config$users) {
    username <- sys_user
  } else {
    cli::cli_alert_warning("System username not found in config. Manual entry required.")
    username <- rstudioapi::askForPassword("Please enter your database username.")
  }

  #retry mechanism
  con <- NULL
  for (attempt in 1:max_attempts) {

    con <- tryCatch({
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = config$server$host,
        port = config$server$port,
        dbname = config$server$database_FISH,
        user = username,
        password = rstudioapi::askForPassword("Please enter your password.")
      )
    }, error = function(e) {
      message(paste("Attempt", attempt, "failed:", conditionMessage(e)))
      Sys.sleep(delay_seconds)
      NULL
    })
    if (!is.null(con)) break
  }

  #check connection
  if (is.null(con) || !DBI::dbIsValid(con)) {
    stop("Failed to establish connection after ", max_attempts, " attempts.")
  } else {
    cli::cli_alert_success("Database connection established.")
  }

  return(con)
}
