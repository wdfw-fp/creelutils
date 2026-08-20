# Get fishery lookup table

Simple wrapper to query the fishery_lut table.

## Usage

``` r
fishery_lut(conn = NULL, fishery_name = NULL)
```

## Arguments

- conn:

  A valid database connection from
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)

- fishery_name:

  placeholder - need to standardize argument definitions across
  functions

## Value

Tibble of fishery names with year, start dates, end dates, and metadata
