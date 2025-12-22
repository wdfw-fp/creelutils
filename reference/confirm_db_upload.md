# Confirm database upload

Confirms upload of model estimates by querying model_analysis_lut for
session analysis_id.

## Usage

``` r
confirm_db_upload(con, analysis_lut)
```

## Arguments

- con:

  Connection to WDFW PostgreSQL database made with DBI-compliant
  RPostgres package. 'con' created by establish_db_con.R function.

- analysis_lut:

  Data frame containing session-specific analysis_id and associated
  metadata created by generate_analysis_lut.R

## Value

nothing returned.
