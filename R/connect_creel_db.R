#' Connect to the WDFW freshwater creel database
#'
#' @description
#' Establishes a connection to the WDFW PostgreSQL creel database using
#' `RPostgres`. Credentials are retrieved from the OS keychain via the
#' `keyring` package, which also supports a GitHub Actions environment variable
#' backend for automated runs.
#'
#' @section First-time setup:
#' Run the following once per machine to store your credentials in the OS
#' keychain (Windows Credential Manager):
#' ```r
#' keyring::key_set(service = "creel_estimates", username = "your-db-username")
#' ```
#' You will be prompted for your password. After that, `connect_creel_db()`
#' retrieves credentials silently on every subsequent call.
#'
#' @param db_env `"prod"` (default) connects to the production database.
#'   `"test"` connects to the test database. Credentials are the same for both;
#'   write permissions are more restricted on the test server.
#' @param config_path Path to the `config.yml` file containing server
#'   connection details. When `NULL` (the default), checks the
#'   `CREELUTILS_CONFIG_PATH` environment variable first, then falls back to
#'   `rappdirs::user_config_dir("creelutils")`.
#'
#' @return A `DBI` connection object to a PostgreSQL database. It is
#'   conventional to assign this to `con`:
#'   ```r
#'   con <- connect_creel_db()
#'   ```
#'   Remember to disconnect when finished: `DBI::dbDisconnect(con)`.
#'
#' @family internal_data
#' @export
#'
#' @examples
#' \dontrun{
#' # Production (default)
#' con <- connect_creel_db()
#'
#' # Test server
#' con <- connect_creel_db(db_env = "test")
#'
#' DBI::dbDisconnect(con)
#' }
connect_creel_db <- function(
    db_env = c("prod", "test"),
    config_path = NULL
) {
  db_env <- match.arg(db_env)

  # Connection priority
  # 1 - config_path arg supplied, 2 - env var for GH Actions workflow, 3 - config.yml file in .config dotfolder
  if (is.null(config_path)) {
    env_path <- Sys.getenv("CREELUTILS_CONFIG_PATH", unset = "")
    config_path <- if (nzchar(env_path)) {
      env_path
    } else {
      file.path(Sys.getenv("USERPROFILE", unset = Sys.getenv("HOME")), ".config", "creelutils", "config.yml")
    }
  }

  # -- 1. Read and validate config --------------------------------------------

  if (!file.exists(config_path)) {
    cli::cli_abort(c(
      "Config file not found at {.path {config_path}}.",
      "i" = "Place your {.file config.yml} in
             {.path ~/.config/creelutils/},
             or supply the path via {.arg config_path}.",
      "i" = "You can also set the {.envvar CREELUTILS_CONFIG_PATH} environment
             variable."
    ))
  }

  config <- yaml::read_yaml(config_path)

  required_server_fields <- c("host", "port", "database_FISH")
  missing_fields <- setdiff(required_server_fields, names(config$server))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "Config {.file config.yml} is missing required field(s) under {.field server}:",
      "x" = "{.field {missing_fields}}"
    ))
  }

  # -- 2. Select host based on environment ------------------------------------

  # Connect to test server if selected
  if (db_env == "test") {
    if (is.null(config$server$test_host)) { # Verify test_host is available in users config file
      cli::cli_abort(c(
        "No {.field test_host} found in {.path {config_path}}.",
        "i" = "Add a {.field test_host} entry under {.field server} to use the test database."
      ))
    }
    host <- config$server$test_host
    cli::cli_alert_info("Connecting to {.strong test} database.")
  } else {
    # If test not selected, default to prod
    host <- config$server$host
  }

  # -- 3. Resolve credentials via keyring -------------------------------------

  rlang::check_installed(
    "keyring",
    reason = "to retrieve database credentials from the OS keychain"
  )

  keyring_entries <- tryCatch(
    keyring::key_list(service = "creel_estimates"),
    error = function(e) NULL
  )

  if (is.null(keyring_entries) || nrow(keyring_entries) == 0) {
    cli::cli_abort(c(
      "No credentials found in the keychain for service {.val creel_estimates}.",
      "i" = "Run the following once to store your database credentials:",
      " " = "{.run keyring::key_set(service = \"creel_estimates\", username = \"your-db-username\")}",
      "i" = "You will be prompted for your password."
    ))
  }

  username <- keyring_entries$username[[1]]
  password <- tryCatch(
    keyring::key_get(service = "creel_estimates", username = username),
    error = function(e) {
      cli::cli_abort(c(
        "Found keychain entry for {.val creel_estimates} but could not retrieve the password.",
        "i" = "You may need to unlock your keychain or re-run:",
        " " = "{.run keyring::key_set(service = \"creel_estimates\", username = \"{username}\")}"
      ))
    }
  )

  # -- 4. Connect -------------------------------------------------------------

  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      host     = host,
      port     = config$server$port,
      dbname   = config$server$database_FISH,
      user     = username,
      password = password
    ),
    error = function(e) NULL
  )

  # -- 5. Validate and return -------------------------------------------------

  if (!is.null(con) && DBI::dbIsValid(con)) {
    cli::cli_alert_success(
      "Connected to {.strong {db_env}} database as {.val {username}}."
    )
    return(invisible(con))
  }

  cli::cli_abort(c(
    "Failed to establish a database connection.",
    "i" = "Verify you are connected to the agency network or VPN.",
    "i" = "Check that {.path {config_path}} contains the correct host, port,
           and database name.",
    "i" = "If credentials may have changed, update them with:",
    " " = "{.run keyring::key_set(service = \"creel_estimates\", username = \"{username}\")}"
  ))
}
