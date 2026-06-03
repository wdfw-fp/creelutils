# List all available 'fishery name' values

List all available 'fishery name' values

## Usage

``` r
fetch_fishery_names(...)
```

## Arguments

- ...:

  .

## Value

Character vector of "fishery_name" values which represent identifiers
for a given dataset, which is comprised of multiple elements (e.g.,
effort, interview, catch, etc.).

## See also

Other public_data:
[`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md),
[`get_fishery_data()`](https://wdfw-fp.github.io/creelutils/reference/get_fishery_data.md),
[`query_creel_website()`](https://wdfw-fp.github.io/creelutils/reference/query_creel_website.md),
[`search_fishery_name()`](https://wdfw-fp.github.io/creelutils/reference/search_fishery_name.md)

## Examples

``` r
if (FALSE) { # \dontrun{
head(fetch_fishery_names(), n = 10)
} # }
```
