# Connect to the WDFW freshwater creel database

Establishes a connection to the WDFW PostgreSQL creel database using
`RPostgres`. Credentials are retrieved from the OS keychain via the
`keyring` package, which also supports a GitHub Actions environment
variable backend for automated runs.

## Usage

``` r
connect_creel_db(db_env = c("prod", "test"), config_path = NULL)
```

## Arguments

- db_env:

  `"prod"` (default) connects to the production database. `"test"`
  connects to the test database. Credentials are the same for both;
  write permissions are more restricted on the test server.

- config_path:

  Path to the `config.yml` file containing server connection details.
  When `NULL` (the default), checks the `CREELUTILS_CONFIG_PATH`
  environment variable first, then falls back to
  `C:/Users/user-name/.config/creelutils`.

## Value

A `DBI` connection object to a PostgreSQL database. It is conventional
to assign this to `con`:

    con <- connect_creel_db()

Remember to disconnect when finished: `DBI::dbDisconnect(con)`.

## First-time setup

Run the following once per machine to store your credentials in the OS
keychain (Windows Credential Manager):

    keyring::key_set(service = "creel_estimates", username = "your-db-username")

You will be prompted for your password. After that, `connect_creel_db()`
retrieves credentials silently on every subsequent call.

## See also

Other internal_data:
[`fetch_db_table()`](https://wdfw-fp.github.io/creelutils/reference/fetch_db_table.md),
[`fishery_catchgroups()`](https://wdfw-fp.github.io/creelutils/reference/fishery_catchgroups.md),
[`fishery_catchgroups_obs()`](https://wdfw-fp.github.io/creelutils/reference/fishery_catchgroups_obs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Production (default)
con <- connect_creel_db()

# Test server
con <- connect_creel_db(db_env = "test")

DBI::dbDisconnect(con)
} # }
```
