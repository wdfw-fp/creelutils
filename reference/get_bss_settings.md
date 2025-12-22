# Summarize priors, sampler settings, and metadata from a stanfit object

Summarize priors, sampler settings, and metadata from a stanfit object

## Usage

``` r
get_bss_settings(ecg, ecg_fit, inputs_bss, print = FALSE)
```

## Arguments

- ecg:

  Estimate catch group (e.g., Steelhead_Adult_UM_Released)

- ecg_fit:

  A fitted model (stanfit object)

- inputs_bss:

  Inputs of the stan model

- print:

  Logical. Return summary to Console

## Value

List with model priors, sampler settings, and metadata
