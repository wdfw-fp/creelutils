# creelutils 0.2.1

*Released 2026-08-20*

* Add instructions to `fetch_data()` for setting up optional Socrata API token for <https://data.wa.gov> to prevent occasional throttling.

# creelutils 0.2.0

*Released 2026-06-02*

## New features

* Overhaul database connectivity and streamline user experience

  * Store user credentials in their OS credential manager with the `keyring` package (#19).
  
  * Relocate local `config.yml` from working directory of package `creelutils` is being called from to a user-level dotfile stored at `~/.config/creelutils` (#22).
  
  * Implement lazy database connections for `fetch_db_table()` and its wrappers in `R/query_helpers.R` (#26).
  
* New `fetch_data()` serves as a generalized method of querying raw datasets from either the Postgres database directly or from public views hosted at <https://data.wa.gov> (#29, #30).

  * Previously `fetch_dwg()` only allowed downloading from the public data source. The internal option is a more direct approach that does not have the limitation of waiting for the public views to sync on the preset schedule. Both paths now return type-identical output (the internal route includes several additional metadata columns), so database-sourced data feeds cleanly into the downstream CreelEstimates Stan models.

* New `plot_zipcodes()` produces heat maps of angler-reported ZIP codes for individual states and full US (#18).

* New `fishery_catchgroups_obs()` leverages `fishery_catchgroups()` and the raw catch table to produce a tibble of reference catch groups of interest for a given fishery filtered to just those with observed catch data (#20).

* New `catchgroups_to_params()` helps convert from catch group component fields (i.e., species, life_stage, fin_mark, fate) into the vector format used by `wdfw-fp/CreelEstimates/template_scripts/fw_creel.Rmd` and its YAML param `params$est_catch_group` (#28).

## Minor improvements and bug fixes

* Fixed `fishery_catchgroups()` which was querying the wrong database view (#15).

* Added missing `params` argument to `transform_estimates()` to prevent environmental scoping issues when using the ETL (#24).

* Added `bit64` to Suggests (#30).

## Deprecation

* Removed experimental function `render_progress_report()` (#17).

* `fetch_dwg()` was folded into `fetch_data()` and now serves as a wrapper for the new generalized data querying function (#29).

## Documentation enhancements

* Changed `pkgdown.yaml` to trigger on version tags instead of on every pull request and merge (#16).

# creelutils 0.1.1

*Released 2026-03-23*

* `establish_db_con()`: changed default `conn_type` from `"odbc"` to `"config"` to use the faster `RPostgres` driver by default. ODBC is still available via `conn_type = "odbc"`. Note: password prompt from config will block execution in non-interactive (automated) sessions.

# creelutils 0.1.0

*Released 2025-05-08*

* Early development version. Putting the package together with basic structure, documentation, GitHub actions, etc.

*creelutils initialized on 2025-01-31*
