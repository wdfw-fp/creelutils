#' Query database tables
#'
#' @description Helper function to streamline queries of the WDFW Postgres database. It requires a valid database connection. The `dbplyr` package is leveraged to convert R code into SQL.
#'
#' Note: If the 'filter' argument contains a "catch_group" component, the value provided will be parsed into component fields. For example, "Chinook_Adult_AD_Released" is translated to "species_name = 'Chinook', life_stage_name = 'Adult', fin_mark_desc = 'Adclip clip + No other external marks', fate_name = 'Released'".
#'
#' @family internal_data
#' @param con A valid connection to the WDFW PostgreSQL database. @seealso [establish_db_con()]
#' @param schema The database schema of interest. Most freshwater creel tasks use the "creel" schema.
#' @param table The table or view within the database schema that is to be queried.
#' @param filter A `dplyr` style filter which may contain one or more elements. See Examples section for more information.
#' @param show_query Optional argument that when set to logical 'TRUE' will print the interpolated SQL query that was sent to the database.
#'
#' @return Tibble of query results returned to R from Postgres database.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Basic table query
#' a <- fetch_db_table(con, "creel", "fishery_location_lut")
#'
#' # Query with a single filter condition
#' b <- fetch_db_table(
#'   con, "creel", "fishery_location_lut",
#'   filter = "survey_type == 'Index'"
#'   )
#'
#' # Query with a vector of filter conditions
#' c <- fetch_db_table(
#'   con, "creel", "fishery_location_lut",
#'   filter = c("survey_type == 'Index'", "section_num == '1'")
#'   )
#' }

fetch_db_table <- function(con, schema, table, filter = NULL, show_query = FALSE) {
  # check connection to database
  if(!DBI::dbIsValid(con)) {
    stop("fetch_db_table error: No database connection provided.")
  }

  # validate inputs
  if (missing(schema) || missing(table) || !nzchar(schema) || !nzchar(table)) {
    stop("fetch_db_table error: Both 'schema' and 'table' must be provided as non-empty strings.")
  }

  # build query
  query <- dplyr::tbl(con, dbplyr::in_schema(schema, table))

  # define lookup for mark_status translation
  mark_status_lookup <- c(
    "UM" = "No Adclip clip + No other external marks",
    "AD" = "Adclip clip + No other external marks"
  )

  # apply filters if provided
  if (!is.null(filter)) {
    if (!is.character(filter)) {
      stop("fetch_db_table error: 'filter' must be a character vector.")
    }

    expanded_filters <- purrr::map(filter, function(f) {
      if (grepl("^catch_group *==", f)) {
        # Extract value after "=="
        catch_group_value <- gsub("['\"]", "", trimws(sub(".*== *", "", f)))

        # split into parts
        parts <- strsplit(catch_group_value, "_")[[1]]

        if (length(parts) != 4) {
          stop("fetch_db_table error: Invalid 'catch_group' format. Expected: Species_LifeStage_MarkStatus_Fate (e.g., 'Coho_Adult_AD_Kept')")
        }

        # assign components
        species <- parts[1]
        life_stage <- parts[2]
        mark_status <- parts[3]
        fate <- parts[4]

        # translate mark status using lut
        mark_status <- mark_status_lookup[[mark_status]]

        # return multiple filter conditions
        return(c(
          sprintf("species_name == '%s'", species),
          sprintf("life_stage_name == '%s'", life_stage),
          sprintf("fin_mark_desc == '%s'", mark_status),
          sprintf("fate_name == '%s'", fate)
        ))
      } else {
        return(f)  # return unchanged filter - catch_group not used
      }
    }) |> unlist()

    # apply filter to query to be sent
    parsed_filters <- purrr::map(expanded_filters, rlang::parse_expr)
    query <- query |> dplyr::filter(!!!parsed_filters)
  }

  # if (!is.null(filter)) {
  #   combined_filter <- paste(filter, collapse = " & ")
  #   query <- query |> dplyr::filter(!!rlang::parse_expr(combined_filter))
  # }

  # optionally show SQL query
  if (show_query) {
    cli::cli_h2("Query sent to database")
    query |> dplyr::show_query()
  }

  # execute query and return results
  result <- query |> dplyr::collect()

  return(result)
}
