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
head(fetch_fishery_names(), n = 10)
#>  [1] "2024 Potholes Reservoir"     "2025 Banks Lake"            
#>  [3] "Baker summer sockeye 2022"   "Baker summer sockeye 2023"  
#>  [5] "Cascade fall salmon 2021"    "Cascade fall salmon 2022"   
#>  [7] "Cascade fall salmon 2023"    "Cascade fall salmon 2024"   
#>  [9] "Cascade spring Chinook 2021" "Cascade spring Chinook 2022"
```
