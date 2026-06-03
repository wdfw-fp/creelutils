# Plot angler ZIP codes

Creates a map displaying the home ZIP codes reported by anglers during
creel interviews, conveying the geographic range and density of angler
origin for a given fishery.

## Usage

``` r
plot_zipcodes(data, type = "wa")
```

## Arguments

- data:

  list, creel dataset where interviewed anglers provided their home zip
  codes (expects `data$interview$zip_code`).

- type:

  character, map extent. Either `"wa"` (default) or `"us"`.
  Case-insensitive.

## Value

A ggplot object
