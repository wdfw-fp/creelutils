#' Fetch raw creel datasets
#'
#' @description
#' Retrieves freshwater recreational fishery creel datasets for a single
#' fishery. Data can be sourced from the WDFW PostgreSQL database
#' (`data_source = "internal"`) or from the public data portal at <https://data.wa.gov>
#' (`data_source = "external"`). Returns a named list of tibbles in a
#' consistent structure regardless of the source.
#'
#' @param conn A valid database connection from [connect_creel_db()]. When
#'   `data_source = "internal"` and `conn = NULL` (default), a connection is
#'   opened automatically via [connect_creel_db()] and closed on exit. Ignored
#'   entirely when `data_source = "external"`.
#' @param fishery_name Character string. The exact fishery name to filter on.
#' @param tables Character vector of data components to retrieve. Defaults to
#'   all eight: `"effort"`, `"ll"`, `"interview"`, `"catch"`, `"closures"`,
#'   `"fishery_manager"`, `"creel_event"`, `"model_catch_group"`. Subset as
#'   needed, e.g. `tables = c("catch", "interview")`.
#' @param data_source Character string, either `"internal"` or `"external"`.
#'   `"internal"` queries the WDFW PostgreSQL database (requires `conn`).
#'   `"external"` downloads from data.wa.gov (no database connection needed).
#'
#' @return A named list of tibbles. Only requested tables are included. Element
#'   names are identical regardless of `data_source`:
#'   `$effort`, `$ll`, `$interview`, `$catch`, `$closures`,
#'   `$fishery_manager`, `$creel_event`, `$model_catch_group`.
#'
#' @details
#' ## Internal path (`data_source = "internal"`)
#' Queries database views via [fetch_db_table()]. The `conn` argument must be a
#' valid connection from [connect_creel_db()]. The `ll` (latitude/longitude)
#' table is derived by filtering `water_body_lut` to water bodies present in the
#' effort data. If `"ll"` is requested without `"effort"`, the effort view is
#' queried internally to resolve water body names but is not returned.
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
#' @importFrom httr GET add_headers content status_code stop_for_status
#' @importFrom dplyr select mutate
#'
#' @examples
#' \dontrun{
#' # External (no database needed)
#' data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
#'                    data_source = "external")
#'
#' # Internal with automatic `connect_creel_db()` call
#' data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
#'                    data_source = "internal")
#'
#' # Subset of tables
#' data <- fetch_data(fishery_name = "Skagit fall salmon 2025",
#'                    tables = c("catch", "interview"),
#'                    data_source = "external")
#' }
fetch_data <- function(
    conn = NULL,
    fishery_name,
    tables = c("effort", "ll", "interview", "catch", "closures",
               "fishery_manager", "creel_event", "model_catch_group"),
    data_source = c("internal", "external")
) {

  # Validate arguments ----

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

  # Dispatch on data_source ----

  if (data_source == "internal") {
    result <- .fetch_data_internal(conn, fishery_name, tables)
  } else {
    result <- .fetch_data_external(fishery_name, tables)
  }

  # Return tables in canonical order ----

  result[intersect(valid_tables, names(result))]
}


# Internal path dispatch function -----------------------------------------------------------------------------------

#' Fetch raw data from internal Postgres database
#'
#' This function queries raw datasets from the creel schema of the WDFW Postgres database
#' @noRd
.fetch_data_internal <- function(conn, fishery_name, tables) {

  # Lazy connection: open internally if none supplied, close on exit

  if (is.null(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  if (!DBI::dbIsValid(conn)) {
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
    effort <- fetch_db_table(conn, "creel", view_map[["effort"]], filter = fishery_filter) |>
      tidyr::drop_na(.data$count_type) |>
      dplyr::select(-dplyr::any_of(c("created_datetime", "modified_datetime")))

    if ("effort" %in% tables) {
      result[["effort"]] <- effort
    }
  }

  # Interview — also needed if creel_event is requested
  needs_interview <- "interview" %in% tables || "creel_event" %in% tables

  if (needs_interview) {
    interview <- fetch_db_table(conn, "creel", view_map[["interview"]], filter = fishery_filter) |>
      dplyr::select(-dplyr::any_of(c("created_datetime", "modified_datetime")))

    if ("interview" %in% tables) {
      result[["interview"]] <- interview
    }
  }

  # ll (water body coordinates) — filtered to water bodies in effort
  if ("ll" %in% tables) {
    water_bodies <- unique(effort$water_body)
    wb_filter <- glue::glue("water_body_desc %in% c({paste0(\"'\", water_bodies, \"'\", collapse = ', ')})")
    result[["ll"]] <- fetch_db_table(conn, "creel", "water_body_lut", filter = wb_filter)
  }

  # Catch — select columns and derive catch_group
  if ("catch" %in% tables) {
    result[["catch"]] <- fetch_db_table(conn, "creel", view_map[["catch"]], filter = fishery_filter) |>
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

  # Closures — select columns
  if ("closures" %in% tables) {
    result[["closures"]] <- fetch_db_table(conn, "creel", view_map[["closures"]], filter = fishery_filter) |>
      dplyr::select(.data$fishery_name, .data$section_num, .data$event_date)
  }

  # Remaining standard tables
  for (tbl in intersect(tables, c("fishery_manager", "model_catch_group"))) {
    result[[tbl]] <- fetch_db_table(conn, "creel", view_map[[tbl]], filter = fishery_filter)
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

    result[["creel_event"]] <- fetch_db_table(conn, "creel", view_map[["creel_event"]], filter = event_filter)
  }

  result
}


# External path dispatch function -----------------------------------------------------------------------------------

#' Fetch raw data from public data portal, data.wa.gov
#'
#' This function queries raw datasets from the public mirrors of database tables
#' made available on https://data.wa.gov
#' @noRd
.fetch_data_external <- function(fishery_name, tables) {

  ## App token setup ----
  app_token <- Sys.getenv("SOCRATA_APP_TOKEN", unset = NA)

  if (is.na(app_token)) {
    cli::cli_warn(c(
      "No SOCRATA_APP_TOKEN found in environment.",
      "i" = "Requests will be unauthenticated and may be throttled.",
      "i" = "See README for setup instructions: {.url https://github.com/wdfw-fp/CreelEstimates}"
    ))
  }

  ## Socrata endpoint mapping ----
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
    effort <- .socrata_get(
      dwg_base$effort,
      paste0("fishery_name='", fishery_name, "'"),
      app_token
    ) |>
      tidyr::drop_na(.data$count_type) |>
      dplyr::select(-.data$created_datetime, -.data$modified_datetime)

    if ("effort" %in% tables) {
      result[["effort"]] <- effort
    }
  }

  # ll (water body coordinates) — filtered to water bodies in effort
  if ("ll" %in% tables) {
    wb_in <- paste0("water_body_desc in('",
      paste0(unique(effort$water_body), collapse = "','"), "')")
    result[["ll"]] <- .socrata_get(dwg_base$water_bodies, wb_in, app_token)
  }

  # Interview — also needed if creel_event is requested
  needs_interview <- "interview" %in% tables || "creel_event" %in% tables

  if (needs_interview) {
    interview <- .socrata_get(
      dwg_base$interview,
      paste0("fishery_name='", fishery_name, "'"),
      app_token
    ) |>
      dplyr::select(-.data$created_datetime, -.data$modified_datetime)

    if ("interview" %in% tables) {
      result[["interview"]] <- interview
    }
  }

  # Catch
  if ("catch" %in% tables) {
    result[["catch"]] <- .socrata_get(
      dwg_base$catch,
      paste0("fishery_name='", fishery_name, "'"),
      app_token
    ) |>
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
    result[["closures"]] <- .socrata_get(
      dwg_base$closures,
      paste0("fishery_name='", fishery_name, "'"),
      app_token
    ) |>
      dplyr::select(.data$fishery_name, .data$section_num, .data$event_date)
  }


  # Fishery manager
  if ("fishery_manager" %in% tables) {
    result[["fishery_manager"]] <- .socrata_get(
      dwg_base$fishery_manager,
      paste0("fishery_name='", fishery_name, "'"),
      app_token
    )
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

    result[["creel_event"]] <- .socrata_get(
      dwg_base$creel_event, where_clause, app_token
    )
  }

  # model_catch_group — not yet available externally
  if ("model_catch_group" %in% tables) {
    message("model_catch_group is not yet published to data.wa.gov. Returning NULL for this table.")
    result[["model_catch_group"]] <- NULL
  }

  result
}


# Socrata HTTP helper function ------------------------------------------------------------------------------------

#' Query a Socrata endpoint with pagination
#' @param base_url API endpoint for a given data component from `dwg_base` in `.fetch_data_external()`
#' @param where_clause Filter supplied to specify a subset of data (e.g., fishery_name)
#' @param app_token Optional user-specific ID stored in that prevents throttling. This is setup once per user
#' and
#' @param limit
#' @return A tibble of all rows matching `where_clause`, paginated until exhausted
#' @noRd
.socrata_get <- function(base_url, where_clause, app_token, limit = 50000L) {

  headers <- if (!is.na(app_token)) {
    httr::add_headers(`X-App-Token` = app_token)
  } else {
    NULL
  }

  offset <- 0L
  pages <- list()


  repeat {
    url <- paste0(
      base_url, "?$where=", where_clause,
      "&$limit=", limit, "&$offset=", offset
    ) |> utils::URLencode()

    response <- httr::GET(url = url, headers)

    if (httr::status_code(response) == 429) {
      cli::cli_abort(c(
        "API request throttled (HTTP 429).",
        "x" = "You are likely missing a SOCRATA_APP_TOKEN.",
        "i" = "Set the token in your .Renviron: {.code SOCRATA_APP_TOKEN=your_token_here}"
      ))
    }

    httr::stop_for_status(response)

    page <- httr::content(response, as = "text", encoding = "UTF-8") |>
      readr::read_csv(show_col_types = FALSE)

    pages <- c(pages, list(page))

    if (nrow(page) < limit) break
    offset <- offset + limit
  }

  dplyr::bind_rows(pages)
}
