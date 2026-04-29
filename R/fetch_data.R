#' Fetch raw datasets from the internal database (alternative to fetch_dwg)
#'
#' @description
#' Queries creel data views from the WDFW PostgreSQL database for a
#' single fishery. Returns a named list with the same structure as [fetch_dwg()],
#' making it a drop-in replacement for sourcing data internally instead of
#' through the public data portal (data.wa.gov).
#'
#' @param conn A valid database connection from [connect_creel_db()]. If `NULL`
#'   (default), a connection is opened internally and closed on exit.
#' @param fishery_name Character string. The exact fishery name to filter on.
#' @param tables Character vector of data components to retrieve. Defaults to
#'   all six: `"effort"`, `"ll"`, `"interview"`, `"catch"`, `"closures"`,
#'   `"fishery_manager"`. Subset as needed, e.g. `tables = c("catch", "interview")`.
#'
#' @return A named list of tibbles, ordered to match [fetch_dwg()] output:
#'   `$effort`, `$ll`, `$interview`, `$catch`, `$closures`, `$fishery_manager`.
#'   Only requested tables are included.
#'
#' @details
#' The `ll` (i.e., lat/long water body coordinates) table is derived by filtering
#' `water_body_lut` to water bodies present in the effort data. If `"ll"` is
#' requested without `"effort"`, the effort view is queried internally to
#' resolve water body names but is not included in the returned list.
#'
#' @family internal_data
#' @export
#'
#' @examples
#' \dontrun{
#' # All tables for a fishery
#' data <- fetch_data(fishery_name = "Skagit fall salmon 2025")
#'
#' # Just catch and interview
#' data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
#'                    tables = c("catch", "interview"))
#'
#' # With an existing connection
#' con <- connect_creel_db()
#' data <- fetch_data(conn = con, fishery_name = "Skagit fall salmon 2025")
#' DBI::dbDisconnect(con)
#' }
fetch_data <- function(
    conn = NULL,
    fishery_name,
    tables = c("effort", "ll", "interview", "catch", "closures", "fishery_manager")
) {

  # -- 0. Validate tables argument --------------------------------------------

  valid_tables <- c("effort", "ll", "interview", "catch", "closures", "fishery_manager")
  unknown <- setdiff(tables, valid_tables)

  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown table{?s} requested: {.val {unknown}}.",
      "i" = "Valid options are: {.val {valid_tables}}"
    ))
  }

  # -- 1. Resolve database connection -----------------------------------------

  if (is.null(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  if (!DBI::dbIsValid(conn)) {
    cli::cli_abort("Database connection is not valid or has been closed. Reconnect with {.fn connect_creel_db}.")
  }

  # -- 2. Define view mapping -------------------------------------------------

  view_map <- c(
    effort          = "vw_analysis_effort_count",
    interview       = "vw_analysis_interview",
    catch           = "vw_analysis_catch",
    closures        = "vw_fishery_closure",
    fishery_manager = "vw_fishery_manager"
  )

  fishery_filter <- glue::glue("fishery_name == '{fishery_name}'")

  # -- 3. Query requested tables ----------------------------------------------

  result <- list()

  # Effort — also needed internally if ll is requested
  needs_effort <- "effort" %in% tables || "ll" %in% tables

  if (needs_effort) {
    effort <- fetch_db_table(conn, "creel", view_map[["effort"]], filter = fishery_filter)

    if ("effort" %in% tables) {
      result[["effort"]] <- effort
    }
  }

  # ll (water body coordinates) — filtered to water bodies in effort
  if ("ll" %in% tables) {
    water_bodies <- unique(effort$water_body)
    wb_filter <- glue::glue("water_body_desc %in% c({paste0(\"'\", water_bodies, \"'\", collapse = ', ')})")
    result[["ll"]] <- fetch_db_table(conn, "creel", "water_body_lut", filter = wb_filter)
  }

  # Remaining standard tables
  for (tbl in intersect(tables, c("interview", "catch", "closures", "fishery_manager"))) {
    result[[tbl]] <- fetch_db_table(conn, "creel", view_map[[tbl]], filter = fishery_filter)
  }

  # -- 4. Return in canonical order -------------------------------------------

  result[intersect(valid_tables, names(result))]
}
