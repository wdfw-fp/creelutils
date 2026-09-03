# Resolve a database connection argument

Centralizes connection handling for functions taking a `conn` argument.
`NULL` opens a lazy connection that is closed when the calling function
exits. An invalid (closed or stale) connection is a loud error, never a
silent reconnect. Prior to v0.3.0, a silent reconnect defaulted as
`connect_creel_db(db_env = "prod")` and could reroute a stale `test`
connection inadvertently to `prod`.

## Usage

``` r
.resolve_conn(conn = NULL, arg = "conn", call = parent.frame())
```

## Arguments

- conn:

  A DBI connection, or `NULL` to open one lazily.

- arg:

  Name of the argument being resolved, for error messages.

- call:

  Caller environment; also where the lazy connection's disconnect
  handler is registered.

## Value

A valid DBI connection.
