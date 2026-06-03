# Get observed catch groups for a fishery

Filters the catch group reference list returned by
[`fishery_catchgroups()`](https://wdfw-fp.github.io/creelutils/reference/fishery_catchgroups.md)
to only those catch groups with observed catch in the dataset.
Optionally retains unobserved catch groups with a `fish_count` of zero,
which can be useful for communicating about expected-but-absent groups.

Catch groups in the reference list may represent combined groups using a
`|` separator within any component field (e.g.,
`Steelhead_Adult_AD|UM|UNK_Released`). These are expanded to their
atomic equivalents before matching against observed catch.

The standard input for `data` is the list object returned by
[`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md).

## Usage

``` r
fishery_catchgroups_obs(conn = NULL, data, include_zero = FALSE)
```

## Arguments

- conn:

  A valid database connection from
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md).

- data:

  A list containing creel dataset components, as returned by
  [`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md).
  Must include `$catch`, `$interview`, and `$fishery_manager` elements.

- include_zero:

  Logical. If `TRUE`, unobserved catch groups are retained with
  `fish_count = 0`. Default `FALSE`.

## Value

A tibble of catch groups from
[`fishery_catchgroups()`](https://wdfw-fp.github.io/creelutils/reference/fishery_catchgroups.md)
with an appended `fish_count` column, filtered to observed catch groups
unless `include_zero = TRUE`.

## See also

Other internal_data:
[`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md),
[`fetch_db_table()`](https://wdfw-fp.github.io/creelutils/reference/fetch_db_table.md),
[`fishery_catchgroups()`](https://wdfw-fp.github.io/creelutils/reference/fishery_catchgroups.md)
