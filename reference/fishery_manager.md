# Get 'fishery manager' table

Simple wrapper to query the vw_fishery_manager table.

## Usage

``` r
fishery_manager(conn = NULL, fishery_name = NULL)
```

## Arguments

- conn:

  A valid database connection from
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)

- fishery_name:

  Optional character string for pattern matching in analysis_name

## Value

Tibble of fishery information that includes fishery name, dates, and
spatial structure (sections and sites).
