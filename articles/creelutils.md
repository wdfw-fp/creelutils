# User Guide

The `creelutils` package facilitates working with freshwater creel data
by providing a toolkit that streamlines accessing data and performing
common tasks in R.

## Accessing public data

Freshwater recreational creel data is made publicly available at
Washington’s official open data portal,
[data.wa.gov](https:://data.wa.gov). Creel datasets are comprised of
multiple elements (e.g., effort, interview, and catch data) that are
grouped in space and time by a “Fishery Name” identifier. This
identifier is an easy way to view a given creel’s full dataset.

For example, the *Skagit winter steelhead 2021* dataset contains effort
counts, angler interviews, etc. conducted on sections of the Skagit and
Sauk rivers from 2021-02-01 to 2021-04-15.

**By Fishery Name**

The function `fetch_dwg` downloads creel data from data.wa.gov by
Fishery Name and is the primary route for accessing a complete dataset
from a fishery.

``` r
library(creelutils)

fishery_name <- "Skagit winter steelhead 2021"
dat <- fetch_dwg(fishery_name)
#> # A tibble: 6 × 2
#>   `Data Component` Records
#>   <chr>              <int>
#> 1 effort              4864
#> 2 ll                     2
#> 3 interview           1860
#> 4 catch                683
#> 5 closures              60
#> 6 fishery_manager       22

#ll stands for latitude and longitude
names(dat)
#> [1] "effort"          "ll"              "interview"       "catch"          
#> [5] "closures"        "fishery_manager"

colnames(dat$effort)
#>  [1] "creel_event_id"       "event_date"           "water_body"          
#>  [4] "project_name"         "fishery_name"         "effort_event_id"     
#>  [7] "location"             "location_id"          "tie_in_indicator"    
#> [10] "count_sequence"       "effort_start_time"    "effort_end_time"     
#> [13] "no_count_reason"      "comments"             "count_type"          
#> [16] "count_quantity"       "location_type"        "survey_type"         
#> [19] "location_season_name" "section_num"          "surveyor_num"        
#> [22] "p_census_bank"        "p_census_boat"        "indirect_census_bank"
#> [25] "direct_census_bank"
```

**What if I don’t know the Fishery Name?**

The function
[`fetch_fishery_names()`](https://wdfw-fp.github.io/creelutils/reference/fetch_fishery_names.md),
which has no arguments, returns a list of all fishery names in the creel
database.

``` r
fetch_fishery_names() |> head(n = 10)
#>  [1] "Baker summer sockeye 2022"   "Baker summer sockeye 2023"  
#>  [3] "Cascade fall salmon 2021"    "Cascade fall salmon 2022"   
#>  [5] "Cascade fall salmon 2023"    "Cascade fall salmon 2024"   
#>  [7] "Cascade spring Chinook 2021" "Cascade spring Chinook 2022"
#>  [9] "Cascade spring Chinook 2023" "Cascade spring Chinook 2024"
```

**What if I know part of the Fishery Name but not the specific syntax?**

The function
[`search_fishery_name()`](https://wdfw-fp.github.io/creelutils/reference/search_fishery_name.md)
can accept a vector of characters or numbers and returns the fishery
names with those values.

``` r
# by water body
search_fishery_name("Humptulips") |> head(n = 5)
#> [1] "Humptulips salmon 2019" "Humptulips salmon 2020" "Humptulips salmon 2021"
#> [4] "Humptulips salmon 2022" "Humptulips salmon 2023"

# by run / species
search_fishery_name("winter steelhead") |> head(n = 5)
#> [1] "Chehalis winter steelhead 2024-25"   "Chehalis Winter Steelhead 2025-26"  
#> [3] "Clearwater winter steelhead 2024-25" "Hoh winter steelhead 2022-23"       
#> [5] "Hoh winter steelhead 2023-24"

# by year
search_fishery_name("2025") |> head(n = 5)
#> [1] "Chehalis Salmon 2025"                
#> [2] "Chehalis Winter Steelhead 2025-26"   
#> [3] "Chum Broodstock Collection 2025"     
#> [4] "Drano Lake salmon and steelhead 2025"
#> [5] "Hoh fall salmon 2025"
```

### Accessing internal data

This section covers operations that require the appropriate credentials
and permissions for WDFW’s Postgres creel database. Selected ‘views’
from the relational database are mirrored on Washington’s official open
data portal, [data.wa.gov](https:://data.wa.gov), for public access.

#### Connecting to database

An internal connection to the Postgres creel database can be made using
the `establish_db_con` function. When called a RStudio UI prompt will
open for the user to enter their password. This requires the proper user
credentials and setup on local computer. If this is your first time
using this method, contact the package maintainer for assistance.

``` r
# con <- establish_db_con()
```
