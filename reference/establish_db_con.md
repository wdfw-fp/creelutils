# Establish database connection

Establishes a connection to the WDFW PostgreSQL database. This process
requires either a configured ODBC DSN or a local `config.yml` file.

## Usage

``` r
establish_db_con(
  conn_type = c("odbc", "config"),
  dsn = "creel_estimates",
  config_path = "config.yml"
)
```

## Arguments

- conn_type:

  Character input denoting either "odbc" or "config" connection type.

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
