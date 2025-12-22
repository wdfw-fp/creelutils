# Get fishery lookup table

Simple wrapper to query the fishery_lut table.

## Usage

``` r
fishery_lut(conn)
```

## Arguments

- conn:

  A valid database connection from
  [`establish_db_con()`](https://wdfw-fp.github.io/creelutils/reference/establish_db_con.md)

## Value

Tibble of fishery names with year, start dates, end dates, and metadata
