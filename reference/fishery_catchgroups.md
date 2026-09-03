# Get 'fishery_catch_groups' view

Simple wrapper to query the vw_fishery_manager table.

## Usage

``` r
fishery_catchgroups(conn = NULL, fishery_name = NULL, observed_only = FALSE)
```

## Arguments

- conn:

  A valid database connection from
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)

- fishery_name:

  Optional character string for pattern matching in analysis_name

- observed_only:

  Logical. If `TRUE`, return only catch with at least one observed fish
  in the fishery's interview data. Existence check only, no counts are
  returned. Default `FALSE`.

## Value

Tibble of catch groups of interest for a given fishery.

## See also

Other internal_data:
[`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md),
[`fetch_db_table()`](https://wdfw-fp.github.io/creelutils/reference/fetch_db_table.md),
[`fishery_catchgroups_obs()`](https://wdfw-fp.github.io/creelutils/reference/fishery_catchgroups_obs.md)
