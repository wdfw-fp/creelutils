# Establish database connection

Establishes a connection to the WDFW PostgreSQL database. Requires
either a local `config.yml` file (default) or a configured ODBC DSN. The
config path uses driver from the `RPostgres` package and is preferred
for performance, particularly during build database writes. **Note:**
when called in a non-interactive session (scheduled render or automated
script), the password prompt will block execution.

## Usage

``` r
establish_db_con(
  conn_type = c("config", "odbc"),
  dsn = "creel_estimates",
  config_path = "config.yml"
)
```

## Arguments

- conn_type:

  `"config"` (default) uses a local `config.yml` + `RPostgres`; `"odbc"`
  uses a system-configured DSN.

- dsn:

  Character string denoting the ODBC domain service name (DSN) to
  connect to.

- config_path:

  File path location of the local 'config.yml' file.

## Value

A `DBI` connection to a PostgreSQL database management system. Recommend
that this object be named "con".

## See also

Other internal_data:
[`fetch_db_table()`](https://wdfw-fp.github.io/creelutils/reference/fetch_db_table.md)
