# Query database tables

Helper function to streamline queries of the WDFW Postgres database. It
requires a valid database connection. The `dbplyr` package is leveraged
to convert R code into SQL.

Note: If the 'filter' argument contains a "catch_group" component, the
value provided will be parsed into component fields. For example,
"Chinook_Adult_AD_Released" is translated to "species_name = 'Chinook',
life_stage_name = 'Adult', fin_mark_desc = 'Adclip clip + No other
external marks', fate_name = 'Released'".

## Usage

``` r
fetch_db_table(con, schema, table, filter = NULL, show_query = FALSE)
```

## Arguments

- con:

  A valid connection to the WDFW PostgreSQL database. @seealso
  [`establish_db_con()`](https://wdfw-fp.github.io/creelutils/reference/establish_db_con.md)

- schema:

  The database schema of interest. Most freshwater creel tasks use the
  "creel" schema.

- table:

  The table or view within the database schema that is to be queried.

- filter:

  A `dplyr` style filter which may contain one or more elements. See
  Examples section for more information.

- show_query:

  Optional argument that when set to logical 'TRUE' will print the
  interpolated SQL query that was sent to the database.

## Value

Tibble of query results returned to R from Postgres database.

## See also

Other internal_data:
[`establish_db_con()`](https://wdfw-fp.github.io/creelutils/reference/establish_db_con.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Basic table query
a <- fetch_db_table(con, "creel", "fishery_location_lut")

# Query with a single filter condition
b <- fetch_db_table(
  con, "creel", "fishery_location_lut",
  filter = "survey_type == 'Index'"
  )

# Query with a vector of filter conditions
c <- fetch_db_table(
  con, "creel", "fishery_location_lut",
  filter = c("survey_type == 'Index'", "section_num == '1'")
  )
} # }
```
