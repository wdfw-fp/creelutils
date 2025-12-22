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
search_fishery_name("gamefish")
#>  [1] "Cascade winter gamefish 2021-22"                    
#>  [2] "Cascade winter gamefish 2023"                       
#>  [3] "Nooksack winter gamefish 2021-22"                   
#>  [4] "Nooksack winter gamefish 2022-23"                   
#>  [5] "North Fork Skykomish summer gamefish 2025"          
#>  [6] "Skagit spring gamefish 2025"                        
#>  [7] "Skagit summer gamefish 2022"                        
#>  [8] "Skagit summer gamefish 2024"                        
#>  [9] "Skagit winter gamefish 2021-22"                     
#> [10] "Skagit winter gamefish 2022-23"                     
#> [11] "Skagit winter gamefish 2024"                        
#> [12] "Skagit winter gamefish 2025"                        
#> [13] "Skykomish summer Chinook and gamefish 2022"         
#> [14] "Skykomish summer Chinook and gamefish 2025"         
#> [15] "Skykomish summer gamefish 2023"                     
#> [16] "Skykomish winter gamefish 2021-22"                  
#> [17] "Skykomish winter gamefish 2022-23"                  
#> [18] "Snoqualmie summer gamefish 2025"                    
#> [19] "South Fork Skykomish summer gamefish 2025"          
#> [20] "South Fork Stillaguamish summer gamefish 2024 upper"
#> [21] "Stillaguamish salmon and gamefish 2022-23"          
#> [22] "Stillaguamish salmon and gamefish 2023-24"          
#> [23] "Stillaguamish salmon and gamefish 2024-25"          
#> [24] "Stillaguamish salmon and gamefish 2025-26"          
#> [25] "Wallace salmon and gamefish 2022-23"                
#> [26] "Wallace salmon and gamefish 2023-24"                
#> [27] "Wallace salmon and gamefish 2024-25"                
search_fishery_name("Humptulips")
#> [1] "Humptulips Salmon 2025"              "Humptulips Winter Steelhead 2025-26"
#> [3] "Humptulips salmon 2019"              "Humptulips salmon 2020"             
#> [5] "Humptulips salmon 2021"              "Humptulips salmon 2022"             
#> [7] "Humptulips salmon 2023"              "Humptulips salmon 2024"             
#> [9] "Humptulips winter steelhead 2024-25"
```
