# creelutils 0.1.1

* `establish_db_con()`: changed default `conn_type` from `"odbc"` to `"config"` 
  to use the faster `RPostgres` driver by default. ODBC is still available via 
  `conn_type = "odbc"`. Note: password prompt from config will block execution in 
  non-interactive (automated) sessions.

# creelutils 0.1.0

* Early development version. Putting the package together with basic structure, documentation, GitHub actions, etc.
