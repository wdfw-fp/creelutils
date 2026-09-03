# Confirm an analysis_id exists in the database

Confirm an analysis_id exists in the database

## Usage

``` r
confirm_db_upload(conn, analysis_id)
```

## Arguments

- conn:

  An open DBI connection to the creel database.

- analysis_id:

  character; session-specific analysis_id created by
  `generate_analysis_lut()`

## Value

Invisibly, TRUE if the analysis_id was found, otherwise FALSE.
