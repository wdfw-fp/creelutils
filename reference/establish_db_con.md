# Establish database connection

Compatibility wrapper for
[`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md).
Retained for backwards compatibility with scripts written prior to
v0.2.0.

The `conn_type` and `dsn` arguments are no longer supported. ODBC
connections have been removed in favor of `RPostgres` + `keyring`. Calls
passing `conn_type` will receive a warning and the argument will be
ignored. New code should use
[`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)
directly.

## Usage

``` r
establish_db_con(...)
```

## Arguments

- ...:

  Arguments passed to
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md).
  `conn_type` and `dsn` are silently dropped with a warning if supplied.

## Value

A `DBI` connection object. See
[`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)
for details.

## See also

[`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)
