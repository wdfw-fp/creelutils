# Produce a creel progress report and summaries

Produces a Creel Progress Report for a given fishery from a standardized
template script. These reports are primarily used for in-season
monitoring of freshwater recreational fisheries and contains high level
summaries of sampling data with preliminary estimates of total catch and
effort.

## Usage

``` r
render_progress_report(fishery_name, bss_used = FALSE)
```

## Arguments

- fishery_name:

  Identifier which represents the spatiotemporal configuration for a
  given dataset with associated fishery closures.

- bss_used:

  Logical TRUE or FALSE that denotes whether estimates were produced by
  the Bayesian state-space model. The default option is FALSE.

## Examples

``` r
if (FALSE) { # \dontrun{
render_progress_report("Skykomish summer Chinook and gamefish 2025")
render_progress_report("Chehalis winter steelhead 2024-25", bss_used = TRUE)
} # }
```
