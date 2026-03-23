#' Establish database connection
#'
#' @description Establishes a connection to the WDFW PostgreSQL database.
#' Requires either a  local `config.yml` file (default) or a configured ODBC DSN.
#' The config path uses driver from the `RPostgres` package and is preferred for performance, particularly during build database writes.
#' **Note:** when called in a non-interactive session (scheduled render or automated script), the password prompt will block execution.
#' @param conn_type `"config"` (default) uses a local `config.yml` + `RPostgres`; `"odbc"` uses a system-configured DSN.
#' @param dsn Character string denoting the ODBC domain service name (DSN) to connect to.
#' @param config_path File path location of the local 'config.yml' file.
#' @family internal_data
#' @return A `DBI` connection to a PostgreSQL database management system. Recommend that this object be named "con".
#' @export
establish_db_con<- function(
    conn_type = c("config", "odbc"), # default order changed in version 0.1.1
    dsn = "creel_estimates",
    config_path = "config.yml"
  ) {

  conn_type <- match.arg(conn_type, choices = c("odbc", "config"))

  # try odbc connection as default or if requested ####
  if (conn_type == "odbc") {
    odbc_available <- requireNamespace("odbc", quietly = TRUE)
    if (!odbc_available) cli::cli_abort("the 'odbc' package is required for ODBC connections.")

    con <- tryCatch({
      DBI::dbConnect(odbc::odbc(), dsn = dsn) #connection
    }, error = function(e) {
      NULL
    })

    # report connection
    if (!is.null(con) && DBI::dbIsValid(con)) { # success
      cli::cli_alert_success(glue::glue("Successfully connected to database via DSN {dsn}."))
      return(invisible(con))
    } else { # fail
      cli::cli_alert_warning("ODBC connnection failed. Will attempt config file fallback.")
    }
  }

  # use local 'config.yml' file as fallback or if requested ####
  if (!file.exists(config_path)) {
    cli::cli_abort(glue::glue("Config file not found at path:"))
  }

  # load config file
  config <- tryCatch({
    yaml::read_yaml(config_path)
  }, error = function(e) {
    cli::cli_abort(glue::glue("Failed to read config file: {conditionMessage(e)}"))
  })

  # validate config inputs
  for (field in c("server", "users")) {
    if (!field %in% names(config)) {
      stop("Config file is missing required field: ", field)
    }
  }
  for (f in c("host", "port", "database_FISH")) {
    if (!f %in% names(config$server)) {
      stop("Config 'server' section missing required field: ", f)
    }
  }

  # get username
  sys_user <- unname(Sys.info()["user"])
  if (sys_user %in% config$users) {
    username <- sys_user
  } else {
    cli::cli_alert_warning("System username not found in config users list. Manual entry required.")
    username <- rstudioapi::askForPassword("Please enter your database username:")
  }
  # get password
  password <- rstudioapi::askForPassword("Please enter your database password:")

  # connect to database
  con <- tryCatch({
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = config$server$host,
      port = config$server$port,
      dbname = config$server$database_FISH,
      user = username,
      password = password
    )
  }, error = function(e) {
    NULL
  })

  # report connection
  if (!is.null(con) && DBI::dbIsValid(con)) {
    cli::cli_alert_success("Database connection established using config file.")
    return(invisible(con))
  } else {
    stop(cli::format_error(c(
      "Failed to establish a database connection.",
      "i" = "Check that you are connected to VPN or the agency network.",
      "i" = "Verify your DSN or config.yml settings."
    )))
  }
}
