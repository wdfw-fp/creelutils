#' Fetch raw creel datasets
#'
#' @description
#' Retrieves freshwater recreational fishery creel datasets for a single
#' fishery. Data can be sourced from the WDFW PostgreSQL database
#' (`data_source = "internal"`) or from the public data portal at data.wa.gov
#' (`data_source = "external"`). Returns a named list of tibbles in a
#' consistent structure regardless of the source.
#'
#' @param con A valid database connection from [connect_creel_db()]. When
#'   `data_source = "internal"` and `con = NULL` (default), a connection is
#'   opened automatically via [connect_creel_db()] and closed on exit. Ignored
#'   entirely when `data_source = "external"`.
#' @param fishery_name Character string. The exact fishery name to filter on.
#' @param tables Character vector of data components to retrieve. Defaults to
#'   all eight: `"effort"`, `"ll"`, `"interview"`, `"catch"`, `"closures"`,
#'   `"fishery_manager"`, `"creel_event"`, `"model_catch_group"`. Subset as
#'   needed, e.g. `tables = c("catch", "interview")`.
#' @param data_source Character string, either `"internal"` or `"external"`.
#'   `"internal"` queries the WDFW PostgreSQL database (requires `con`).
#'   `"external"` downloads from data.wa.gov (no database connection needed).
#'
#' @return A named list of tibbles. Only requested tables are included. Element
#'   names are identical regardless of `data_source`:
#'   `$effort`, `$ll`, `$interview`, `$catch`, `$closures`,
#'   `$fishery_manager`, `$creel_event`, `$model_catch_group`.
#'
#' @details
#' ## Internal path (`data_source = "internal"`)
#' Queries database views via [fetch_db_table()]. The `con` argument must be a
#' valid connection from [connect_creel_db()]. The `ll` table is derived by
#' filtering `water_body_lut` to water bodies present in the effort data. If
#' `"ll"` is requested without `"effort"`, the effort view is queried
#' internally to resolve water body names but is not returned.
#'
#' ## External path (`data_source = "external"`)
#' Downloads CSV data from data.wa.gov Socrata endpoints. No database
#' connection is required or referenced. The `model_catch_group` view is not
#' yet published to data.wa.gov; requesting it returns `NULL` with an
#' informative message.
#'
#' @family data
#' @export
#' @importFrom rlang .data
#' @importFrom utils URLencode
#' @importFrom tidyr drop_na
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate
#'
#' @examples
#' \dontrun{
#' # External (no database needed)
#' data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
#'                    data_source = "external")
#'
#' # Internal with auto-connect
#' con <- connect_creel_db()
#' data <- fetch_data(con = con, fishery_name = "Skagit fall salmon 2025",
#'                    data_source = "internal")
#' DBI::dbDisconnect(con)
#'
#' # Subset of tables
#' data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
#'                    tables = c("catch", "interview"),
#'                    data_source = "external")
#' }
fetch_data <- function(
    con = NULL,
    fishery_name,
    tables = c("effort", "ll", "interview", "catch", "closures",
               "fishery_manager", "creel_event", "model_catch_group"),
    data_source = c("internal", "external")
) {

  # -- 0. Validate arguments ---------------------------------------------------

  data_source <- match.arg(data_source)

  valid_tables <- c("effort", "ll", "interview", "catch", "closures",
                    "fishery_manager", "creel_event", "model_catch_group")
  unknown <- setdiff(tables, valid_tables)

  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown table{?s} requested: {.val {unknown}}.",
      "i" = "Valid options are: {.val {valid_tables}}"
    ))
  }

  # -- 1. Dispatch on data_source ----------------------------------------------

  if (data_source == "internal") {
    result <- .fetch_data_internal(con, fishery_name, tables)
  } else {
    result <- .fetch_data_external(fishery_name, tables)
  }

  # -- 2. Return in canonical order --------------------------------------------

  result[intersect(valid_tables, names(result))]
}


# ==============================================================================
# Internal path
# ==============================================================================

.fetch_data_internal <- function(con, fishery_name, tables) {

  # Lazy connection: open internally if none supplied, close on exit

  if (is.null(con)) {
    con <- connect_creel_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Database connection is not valid or has been closed. Reconnect with {.fn connect_creel_db}.")
  }

  # View mapping
  view_map <- c(
    effort            = "vw_analysis_effort_count",
    interview         = "vw_analysis_interview",
    catch             = "vw_analysis_catch",
    closures          = "vw_fishery_closure",
    fishery_manager   = "vw_fishery_manager",
    creel_event       = "vw_analysis_creel_event",
    model_catch_group = "vw_model_catch_group"
  )

  fishery_filter <- glue::glue("fishery_name == '{fishery_name}'")

  result <- list()

  # Effort — also needed if ll or creel_event is requested
  needs_effort <- "effort" %in% tables || "ll" %in% tables || "creel_event" %in% tables

  if (needs_effort) {
    effort <- fetch_db_table(con, "creel", view_map[["effort"]], filter = fishery_filter)

    if ("effort" %in% tables) {
      result[["effort"]] <- effort
    }
  }

  # Interview — also needed if creel_event is requested
  needs_interview <- "interview" %in% tables || "creel_event" %in% tables

  if (needs_interview) {
    interview <- fetch_db_table(con, "creel", view_map[["interview"]], filter = fishery_filter)

    if ("interview" %in% tables) {
      result[["interview"]] <- interview
    }
  }

  # ll (water body coordinates) — filtered to water bodies in effort
  if ("ll" %in% tables) {
    water_bodies <- unique(effort$water_body)
    wb_filter <- glue::glue("water_body_desc %in% c({paste0(\"'\", water_bodies, \"'\", collapse = ', ')})")
    result[["ll"]] <- fetch_db_table(con, "creel", "water_body_lut", filter = wb_filter)
  }

  # Remaining standard tables (these views all have fishery_name)
  for (tbl in intersect(tables, c("catch", "closures", "fishery_manager",
                                  "model_catch_group"))) {
    result[[tbl]] <- fetch_db_table(con, "creel", view_map[[tbl]], filter = fishery_filter)
  }

  # creel_event — filter by water bodies and date range from effort + interview
  if ("creel_event" %in% tables) {
    water_bodies <- unique(c(effort$water_body, interview$water_body))
    all_dates <- c(effort$event_date, interview$event_date)
    min_date <- min(all_dates, na.rm = TRUE)
    max_date <- max(all_dates, na.rm = TRUE)

    event_filter <- c(
      glue::glue("water_body %in% c({paste0(\"'\", water_bodies, \"'\", collapse = ', ')})"),
      glue::glue("event_date >= '{min_date}'"),
      glue::glue("event_date <= '{max_date}'")
    )

    result[["creel_event"]] <- fetch_db_table(con, "creel", view_map[["creel_event"]], filter = event_filter)
  }

  result
}


# ==============================================================================
# External path (data.wa.gov)
# ==============================================================================

.fetch_data_external <- function(fishery_name, tables) {

  # Socrata endpoint mapping
  dwg_base <- list(
    effort          = "https://data.wa.gov/resource/h9a6-g38s.csv",
    interview       = "https://data.wa.gov/resource/rpax-ahqm.csv",
    catch           = "https://data.wa.gov/resource/6y4e-8ftk.csv",
    water_bodies    = "https://data.wa.gov/resource/nbd2-vdmz.csv",
    closures        = "https://data.wa.gov/resource/6zm6-iep6.csv",
    fishery_manager = "https://data.wa.gov/resource/vkjc-s5u8.csv",
    creel_event     = "https://data.wa.gov/resource/ui95-axtn.csv"
  )

  result <- list()

  # Effort — also needed if ll or creel_event is requested
  needs_effort <- "effort" %in% tables || "ll" %in% tables || "creel_event" %in% tables

  if (needs_effort) {
    effort <- paste0(
      dwg_base$effort,
      "?$where=fishery_name='", fishery_name, "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = FALSE) |>
      tidyr::drop_na(.data$count_type) |>
      dplyr::select(-.data$created_datetime, -.data$modified_datetime)

    if ("effort" %in% tables) {
      result[["effort"]] <- effort
    }
  }

  # ll (water body coordinates) — filtered to water bodies in effort
  if ("ll" %in% tables) {
    result[["ll"]] <- paste0(
      dwg_base$water_bodies,
      "?$where=water_body_desc in('",
      paste0(unique(effort$water_body), collapse = "','"),
      "')&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = FALSE)
  }

  # Interview — also needed if creel_event is requested
  needs_interview <- "interview" %in% tables || "creel_event" %in% tables

  if (needs_interview) {
    interview <- paste0(
      dwg_base$interview,
      "?$where=fishery_name='", fishery_name, "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = FALSE) |>
      dplyr::select(-.data$created_datetime, -.data$modified_datetime)

    if ("interview" %in% tables) {
      result[["interview"]] <- interview
    }
  }

  # Catch
  if ("catch" %in% tables) {
    result[["catch"]] <- paste0(
      dwg_base$catch,
      "?$where=fishery_name='", fishery_name, "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = FALSE) |>
      dplyr::select(
        .data$interview_id, .data$catch_id, .data$species, .data$run,
        .data$life_stage, .data$fin_mark, .data$sex, .data$fork_length_cm,
        .data$fate, .data$fish_count
      ) |>
      dplyr::mutate(
        catch_group = paste(.data$species, .data$life_stage, .data$fin_mark,
                            .data$fate, sep = "_")
      )
  }

  # Closures
  if ("closures" %in% tables) {
    result[["closures"]] <- paste0(
      dwg_base$closures,
      "?$where=fishery_name='", fishery_name, "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = FALSE) |>
      dplyr::select(.data$fishery_name, .data$section_num, .data$event_date)
  }


  # Fishery manager
  if ("fishery_manager" %in% tables) {
    result[["fishery_manager"]] <- paste0(
      dwg_base$fishery_manager,
      "?$where=fishery_name='", fishery_name, "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = FALSE)
  }

  # Creel event — filter by water bodies and date range from effort + interview
  if ("creel_event" %in% tables) {
    water_bodies <- unique(c(effort$water_body, interview$water_body))
    all_dates <- c(effort$event_date, interview$event_date)
    min_date <- min(all_dates, na.rm = TRUE)
    max_date <- max(all_dates, na.rm = TRUE)

    wb_clause <- paste0("water_body in('", paste0(water_bodies, collapse = "','"), "')")
    date_clause <- paste0("event_date >= '", min_date, "' AND event_date <= '", max_date, "'")
    where_clause <- paste0(wb_clause, " AND ", date_clause)

    result[["creel_event"]] <- paste0(
      dwg_base$creel_event,
      "?$where=", where_clause, "&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = FALSE)
  }

  # model_catch_group — not yet available externally
  if ("model_catch_group" %in% tables) {
    message("model_catch_group is not yet published to data.wa.gov. Returning NULL for this table.")
    result[["model_catch_group"]] <- NULL
  }

  result
}
