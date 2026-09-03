# Combine catch group components

Accepts a dataframe which must contain four columns ("species",
"life_stage", "fin_mark", "fate"). These columns are combined into a
single string for each row where the component columns are separated by
an underscore "\_". `NA` values are forced to 'NA' characters.

## Usage

``` r
combine_catch_group(df)
```

## Arguments

- df:

  data frame of catch groups

## Value

character string of combined catch groups
