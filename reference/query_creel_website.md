# Query the creel website for specific combinations of data

Queries the website storing the creel data, which uses the Socrata API
(e.g., https://dev.socrata.com/docs/queries/where)

## Usage

``` r
query_creel_website(table_name, query_list, limit = 1e+05)
```

## Arguments

- table_name:

  Name of table (e.g., catch, interview, effort)

- query_list:

  Basically [`filter()`](https://rdrr.io/r/stats/filter.html) statements
  as a list of named items. Item name matches the column name of the
  table, and then item should either be an atomic to be matched exactly,
  or a vector for matching multiple options. See details.

- limit:

  maximum number of rows to return. Numeric, defaults to 100,000

## Value

tibble of creel data

## Details

The query is focused on filtering, and is designed to be written as an R
list, with any number of conditions in it. For example, to return only
data in which `fishery_name` is "Baker summer sockeye 2022", the query
list should include `fishery_name = "Baker summer sockeye 2022"`. And to
return only data for species "Kokanee" or "Rainbow Trout", the query
list should also include `species = c("Kokanee", "Rainbow Trout")`.

## See also

Other public_data:
[`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md),
[`fetch_fishery_names()`](https://wdfw-fp.github.io/creelutils/reference/fetch_fishery_names.md),
[`get_fishery_data()`](https://wdfw-fp.github.io/creelutils/reference/get_fishery_data.md),
[`search_fishery_name()`](https://wdfw-fp.github.io/creelutils/reference/search_fishery_name.md)

## Examples

``` r
dat <- query_creel_website(
  table_name = "catch",
  query_list = list( # each item in list should be named for a column in the table
    fishery_name = "Baker summer sockeye 2022",
    fin_mark = "UM",
    species = c("Kokanee", "Rainbow Trout")
    )
   )
head(dat)
#> # A tibble: 6 × 42
#>   creel_event_id        event_date          water_body project_name fishery_name
#>   <chr>                 <dttm>              <chr>      <chr>        <chr>       
#> 1 E8B234ED-8173-41DB-B… 2022-08-28 00:00:00 Baker Lake District 14  Baker summe…
#> 2 4E8E7F1D-D41F-4FB9-B… 2022-08-27 00:00:00 Baker Lake District 14  Baker summe…
#> 3 4E8E7F1D-D41F-4FB9-B… 2022-08-27 00:00:00 Baker Lake District 14  Baker summe…
#> 4 88C7E889-48E0-41D1-8… 2022-08-14 00:00:00 Baker Lake District 14  Baker summe…
#> 5 7F52BAD4-7C73-48A3-B… 2022-08-05 00:00:00 Baker Lake District 14  Baker summe…
#> 6 D729D3DF-0C24-4414-A… 2022-07-20 00:00:00 Baker Lake District 14  Baker summe…
#> # ℹ 37 more variables: catch_id <chr>, interview_id <chr>, species <chr>,
#> #   run <lgl>, gear_type <chr>, fate <chr>, life_stage <chr>, fin_mark <chr>,
#> #   sex <chr>, maturity <lgl>, fork_length_cm <dbl>, total_length_in <lgl>,
#> #   fish_processing_status <chr>, cwt_detection_status <chr>,
#> #   pit_detection_status <chr>, fish_count <dbl>, scale_card_num <dbl>,
#> #   scale_card_pos <dbl>, dna_code <lgl>, pit_code <lgl>, snout_code <lgl>,
#> #   scale_age_code <lgl>, scale_pattern <lgl>, comments <chr>, …
```
