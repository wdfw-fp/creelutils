#' Download creel datasets from data.wa.gov (or the internal database)
#'
#' Retrieves freshwater recreational fishery creel datasets. By default,
#' downloads from the public data portal (data.wa.gov). If a database
#' connection is supplied via `con`, data is pulled from the WDFW PostgreSQL
#' database instead.
#'
#' @family public_data
#' @param fishery_name Identifier which represents the spatiotemporal
#'   configuration for a given dataset with associated fishery closures.
#' @param con Optional database connection from [connect_creel_db()]. When
#'   provided, data is sourced from the internal database rather than
#'   data.wa.gov. Defaults to `NULL` (external).
#' @param print Logical TRUE/FALSE that toggles whether a summary table prints
#'   in the console reporting the number of rows per table downloaded. Useful
#'   as a quick check to see if anything was downloaded.
#' @importFrom purrr map_int
#' @importFrom tibble tibble
#' @importFrom cli cli_abort
#'
#' @return A named list of tibbles: `$effort`, `$ll`, `$interview`, `$catch`,
#'   `$closures`, `$fishery_manager`.
#' @export
#'
#' @examples
#' \dontrun{
#' # External (default)
#' dwg <- fetch_dwg("Skagit winter steelhead 2021")
#'
#' # Using an existing DB connection
#' con <- connect_creel_db()
#' dwg <- fetch_dwg("Skagit winter steelhead 2021", con = con)
#' DBI::dbDisconnect(con)
#' }
fetch_dwg <- function(fishery_name, con = NULL, print = FALSE) {

  data_source <- if (!is.null(con)) "internal" else "external"

  dwg <- fetch_data(
    con         = con,
    fishery_name = fishery_name,
    tables      = c("effort", "ll", "interview", "catch", "closures", "fishery_manager"),
    data_source = data_source
  )

  # create summary table to show amount of data downloaded
  summary_table <- tibble::tibble(
    `Data Component` = names(dwg),
    Records = purrr::map_int(dwg, nrow)
  )

  if(all(summary_table$Records == 0)) {
    cli::cli_abort("fetch_dwg error: No data downloaded. Check that the 'fishery_name' is valid.")
  }

  # print = TRUE
  if(print) {print(summary_table)}

  return(dwg)
}
