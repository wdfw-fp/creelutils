# Get 'fishery_catch_groups' view

Simple wrapper to query the vw_fishery_manager table.

## Usage

``` r
fishery_catchgroups(conn, fishery_name = NULL)
```

## Arguments

- conn:

  A valid database connection from
  [`establish_db_con()`](https://wdfw-fp.github.io/creelutils/reference/establish_db_con.md)

- fishery_name:

  Optional character string for pattern matching in analysis_name

## Value

Tibble of catch groups of interest for a given fishery.
