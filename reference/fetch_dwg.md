# Download creel datasets from data.wa.gov (or the internal database)

Retrieves freshwater recreational fishery creel datasets. By default,
downloads from the public data portal (data.wa.gov). If a database
connection is supplied via `conn`, data is pulled from the WDFW
PostgreSQL database instead.

## Usage

``` r
fetch_dwg(fishery_name, conn = NULL, print = FALSE)
```

## Arguments

- fishery_name:

  Identifier which represents the spatiotemporal configuration for a
  given dataset with associated fishery closures.

- conn:

  Optional database connection from
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md).
  When provided, data is sourced from the internal database rather than
  data.wa.gov. Defaults to `NULL` (external).

- print:

  Logical TRUE/FALSE that toggles whether a summary table prints in the
  console reporting the number of rows per table downloaded. Useful as a
  quick check to see if anything was downloaded.

## Value

A named list of tibbles: `$effort`, `$ll`, `$interview`, `$catch`,
`$closures`, `$fishery_manager`.

## See also

Other public_data:
[`fetch_fishery_names()`](https://wdfw-fp.github.io/creelutils/reference/fetch_fishery_names.md),
[`get_fishery_data()`](https://wdfw-fp.github.io/creelutils/reference/get_fishery_data.md),
[`query_creel_website()`](https://wdfw-fp.github.io/creelutils/reference/query_creel_website.md),
[`search_fishery_name()`](https://wdfw-fp.github.io/creelutils/reference/search_fishery_name.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# External (default)
dwg <- fetch_dwg("Skagit winter steelhead 2021")

# Using an existing DB connection
con <- connect_creel_db()
dwg <- fetch_dwg("Skagit winter steelhead 2021", conn = con)
DBI::dbDisconnect(con)
} # }
```
