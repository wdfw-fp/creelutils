# Drop sections from creel survey data

Removes specified sections from all components of a creel survey
dataset, including interview, effort, catch, and days tables.

## Usage

``` r
drop_section(data, section)
```

## Arguments

- data:

  A list containing creel survey data with required tables: `interview`,
  `effort`, `catch`, `days`, and `fishery_manager`.

- section:

  Numeric vector of section numbers to drop. Invalid section numbers are
  ignored with a warning. Must leave at least one section remaining.

## Value

A modified list with the same structure as `data`, with specified
sections removed from all applicable tables. Also removes corresponding
`open_section_X` columns from the `days` table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Drop a single section
data_filtered <- drop_section(data, section = 3)

# Drop multiple sections
data_filtered <- drop_section(data, section = c(1, 2))
} # }
```
