# Fetch raw creel datasets

Retrieves freshwater recreational fishery creel datasets for a single
fishery. Data can be sourced from the WDFW PostgreSQL database
(`data_source = "internal"`) or from the public data portal at
<https://data.wa.gov> (`data_source = "external"`). Returns a named list
of tibbles in a consistent structure regardless of the source.

## Usage

``` r
fetch_data(
  conn = NULL,
  fishery_name,
  tables = c("effort", "ll", "interview", "catch", "closures", "fishery_manager",
    "creel_event", "model_catch_group"),
  data_source = c("internal", "external")
)
```

## Arguments

- conn:

  A valid database connection from
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md).
  When `data_source = "internal"` and `conn = NULL` (default), a
  connection is opened automatically via
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)
  and closed on exit. Ignored entirely when `data_source = "external"`.

- fishery_name:

  Character string. The exact fishery name to filter on.

- tables:

  Character vector of data components to retrieve. Defaults to all
  eight: `"effort"`, `"ll"`, `"interview"`, `"catch"`, `"closures"`,
  `"fishery_manager"`, `"creel_event"`, `"model_catch_group"`. Subset as
  needed, e.g. `tables = c("catch", "interview")`.

- data_source:

  Character string, either `"internal"` or `"external"`. `"internal"`
  queries the WDFW PostgreSQL database (requires `conn`). `"external"`
  downloads from data.wa.gov (no database connection needed).

## Value

A named list of tibbles. Only requested tables are included. Element
names are identical regardless of `data_source`: `$effort`, `$ll`,
`$interview`, `$catch`, `$closures`, `$fishery_manager`, `$creel_event`,
`$model_catch_group`.

## Details

### Internal path (`data_source = "internal"`)

Queries database views via
[`fetch_db_table()`](https://wdfw-fp.github.io/creelutils/reference/fetch_db_table.md).
The `conn` argument must be a valid connection from
[`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md).
The `ll` (latitude/longitude) table is derived by filtering
`water_body_lut` to water bodies present in the effort data. If `"ll"`
is requested without `"effort"`, the effort view is queried internally
to resolve water body names but is not returned.

### External path (`data_source = "external"`)

Downloads CSV data from data.wa.gov Socrata endpoints. No database
connection is required or referenced. The `model_catch_group` view is
not yet published to data.wa.gov; requesting it returns `NULL` with an
informative message.

### (Optional) Socrata SODA API Token Setup

<https://www.data.wa.gov/> can occasionally throttle data requests. This
rate limitation is intermittent and may cause `fetch_data` to fail. Each
user may register for an app token that will prevent this from
occurring. It only takes a couple of minutes.

Step 1: Register for an app token

- Create a free account at data.wa.gov —\> click Sign In -\> Sign Up in
  the top right corner

- Once logged in, navigate to your profile -\> Developer Settings -\>
  Create New App Token

- Fill in Application Name (e.g., CreelEstimates) and Description. All
  other fields can be left blank

- Click Save and copy the App Token value (not the Secret Token)

Step 2: Store the token in your R environment

- Open your .Renviron file: `usethis::edit_r_environ()`

- Add the following line, then save and close the file:
  `SOCRATA_APP_TOKEN=your_token_here`

- Restart R, then confirm the token is accessible:
  `Sys.getenv("SOCRATA_APP_TOKEN")`

  - Should return your token, not ""

WARNING: `Renviron` is stored locally and is not tracked by git.

## Examples

``` r
if (FALSE) { # \dontrun{
# External (no database needed)
data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
                   data_source = "external")

# Internal with automatic `connect_creel_db()` call
data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
                   data_source = "internal")

# Subset of tables
data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
                   tables = c("catch", "interview"),
                   data_source = "external")
} # }
```
