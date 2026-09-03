# creelutils 0.3.0

*Released 2026-09-03*

Large update to how catch groups are queried in from the database and how model estimates are uploaded back the creel database.

## New features

* ETL now allows estimates of "combined" catch groups to be uploaded to the creel database (e.g., Coho_Adult|Jack_AD|UM_Kept). Previously, CreelEstimates could process them but the database model estimates tables and the ETL were incompatible. See #35 and companion update at [CreelEstimates#83](https://github.com/dfw-wa/CreelEstimates/pull/83).

* Updated `fishery_catchgroups()` queries model catch group lookup table and can internally filter to only catch groups with observed catch. The new argument 'observed_only' (default FALSE) leverages an internal helper `.observed_catch_group_ids()` rather than in a separate step (#35).

  * The now defunct `fishery_catchgroups_obs()` is set to be removed in a future update.

* New `combine_catch_group()` streamlines converting catch group component fields(i.e., species, life stage, fin mark, fate) into the concatenated form separated by underscores (e.g., Coho_Adult_AD_Kept) (#35).

* Updated `transform_estimates()` to further standardize naming conventions used across PE and BSS model types for the fields 'estimate_category' and 'estimate_type'. Several 'estimate_type' categories were dropped from the stratum and total tables. Those removed from the total table distracted from primary intent of being a high-level catch/effort summary. Those removed from the stratum table included BSS model convergence diagnostics that were only ever evaluated at the total level, leading to thousands of rows per analysis_id that were never looked at. The volume of rows for BSS stratum estimates has subsequently decreased by ~30-40% (#35).

* New `finalize_analysis_lut()` consolidates several steps that were occurring within `export_estimates()` into a single process that extends from the analysis session-specific metadata created by CreelEstimates' `generate_analysis_lut()`. Several additional metadata columns were also added to the 'analysis_lut' (#35).
  
## Minor improvements and bug fixes

* `export_estimates()` now calls `write_lut`, `write_stratum`, and `write_total` with `DBI::dbWithTransaction()`. This ensures that all rows are successfully appended to the database tables as a batch. If expected row counts differ or the database connection drops during the transaction, all three tables are rolled back as a unit.

* New internal function `.resolve_conn()` streamlines how lazy data connections are opened and how stale connections passed into a querying function are handled. Previously `fetch_db_table()` and its wrappers independently validated connections from `connect_creel_db()` (#35). 

  * A stale or closed connection provided to an argument will now produce an informative error rather than opening a connection to 'prod' by default. This prevents a bug where a connection to 'test' could silently change to a connection to 'prod' that produced confusing results.

## Documentation enhancements

* Add "Git Workflow & Conventions" article (#39). This describes basic coding conventions we have adopted, shows examples of routine usage, and covers version releases and continuous integration.

# creelutils 0.2.1

*Released 2026-08-20*

* Add instructions to `fetch_data()` for setting up optional Socrata API token for <https://data.wa.gov> to prevent occasional throttling (#38).

* `fishery_lut()` helper function now has a 'fishery_name' argument for filtering to a specific fishery rather than returning the full table (#34)

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
