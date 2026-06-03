# Find fishery names from a partial names

Helps identify options for
[`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md).

## Usage

``` r
search_fishery_name(fishery_partial)
```

## Arguments

- fishery_partial:

  Partial fishery name. Can take regular expressions, ignores
  capitalization.

## Value

Character vector of 'fishery_name' identifiers containing the partial
value.

## See also

Other public_data:
[`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md),
[`fetch_fishery_names()`](https://wdfw-fp.github.io/creelutils/reference/fetch_fishery_names.md),
[`get_fishery_data()`](https://wdfw-fp.github.io/creelutils/reference/get_fishery_data.md),
[`query_creel_website()`](https://wdfw-fp.github.io/creelutils/reference/query_creel_website.md)

## Examples

``` r
if (FALSE) { # \dontrun{
search_fishery_name("gamefish")
search_fishery_name("Humptulips")
} # }
```
