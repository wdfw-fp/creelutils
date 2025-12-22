# Get model estimates

Fetch model estimates for a given analysis. Defaults to total
(aggregated) estimates. Stratum-level estimates require at least one
filter to prevent accidentally pulling the entire large table.

## Usage

``` r
model_estimates(
  conn = NULL,
  analysis_id = NULL,
  scale = c("total", "stratum"),
  ...
)
```

## Arguments

- conn:

  A valid database connection.

- analysis_id:

  Character string for the analysis_id to retrieve estimates for

- scale:

  Either "total" (default, aggregated) or "stratum" (granular)

- ...:

  Additional filter conditions passed to
  [`fetch_db_table()`](https://wdfw-fp.github.io/creelutils/reference/fetch_db_table.md)

## Value

Tibble of model estimates
