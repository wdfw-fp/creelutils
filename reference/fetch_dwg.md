# Download creel datasets from data.wa.gov

Download freshwater recreational fishery creel datasets from
Washington's public data warehouse, data.wa.gov. When a valid
'fishery_name' is provided several database views are downloaded (i.e.,
effort, interview, catch, water bodies, closures, and a )

## Usage

``` r
fetch_dwg(fishery_name)
```

## Arguments

- fishery_name:

  Identifier which represents the spatiotemporal configuration for a
  given dataset with associated fishery closures.

## Value

dwg, a list object containing a dataframe for each database view
downloaded (e.g., effort, interview, catch)

## See also

Other public_data:
[`fetch_fishery_names()`](https://wdfw-fp.github.io/creelutils/reference/fetch_fishery_names.md),
[`get_fishery_data()`](https://wdfw-fp.github.io/creelutils/reference/get_fishery_data.md),
[`query_creel_website()`](https://wdfw-fp.github.io/creelutils/reference/query_creel_website.md),
[`search_fishery_name()`](https://wdfw-fp.github.io/creelutils/reference/search_fishery_name.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dwg <- fetch_dwg("Skagit winter steelhead 2021")
} # }
```
