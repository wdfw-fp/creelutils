# Obtain and format interview, catch, and effort data for sets of fishery-years

Based on code written for descriptive_statistics.R in the `creelreview`
package. Streamlines the process of getting creel data in useful format.

## Usage

``` r
get_fishery_data(fishery_names, years = NULL)
```

## Arguments

- fishery_names:

  Character string of fishery name or vector of character strings. If
  `years` is provided, `fishery_names` is combined with `year` to create
  fishery names (see example). If `years` is not provided, uses this
  character string or vector of character strings as the exact fishery
  names. Try
  [`search_fishery_name()`](https://wdfw-fp.github.io/creelutils/reference/search_fishery_name.md)
  to see available options.

- years:

  Integer or vector of integers identifying years of data to pull.
  Optional argument, defaults to NULL.

## Value

List with three dataframes: `$interview`, `$catch`, `$effort`,
`$locations`, `$ll`, `$closures`

## See also

Other public_data:
[`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md),
[`fetch_fishery_names()`](https://wdfw-fp.github.io/creelutils/reference/fetch_fishery_names.md),
[`query_creel_website()`](https://wdfw-fp.github.io/creelutils/reference/query_creel_website.md),
[`search_fishery_name()`](https://wdfw-fp.github.io/creelutils/reference/search_fishery_name.md)

## Examples

``` r
if (FALSE) { # \dontrun{
temp <- get_fishery_data(
  fishery_names = "Nisqually salmon",
  years = 2021:2023)
  ## exact definition
  temp <- get_fishery_data(
  fishery_names = c("Skagit fall salmon 2021", "Skagit summer gamefish 2022"))
  ## Using the search_fishery_name() function to choose fisheries
  temp = get_fishery_data(fishery_names = search_fishery_name("cascade winter"))
} # }
```
