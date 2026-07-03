#' Get fishery lookup table
#'
#' @description Simple wrapper to query the fishery_lut table.
#' @param conn A valid database connection from `connect_creel_db()`
#' @param fishery_name placeholder - need to standardize argument definitions across functions
#'
#' @return Tibble of fishery names with year, start dates, end dates, and metadata
#' @export
fishery_lut <- function(
    conn = NULL,
    fishery_name = NULL
  ) {

  # Validate conn input
  # Establish lazy connection if conn not provided
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  # Validate fishery_name input
  if (!is.null(fishery_name) && !rlang::is_string(fishery_name)) {
    cli::cli_abort("{.arg fishery_name} must be a single character string or `NULL`.")
  }

  # Apply fishery_name filter and query database
  name_filter <- NULL
  if (!is.null(fishery_name)) {
    name_filter <- glue::glue("fishery_name == '{fishery_name}'")
  }

  result <- fetch_db_table(
    conn = conn,
    schema = "creel",
    table = "fishery_lut",
    filter = name_filter
  )

  # Evaluate query result
  if (!is.null(fishery_name) && nrow(result) == 0L) {
    cli::cli_abort(
      c(
        "{.val {fishery_name}} matched no rows in {.field creel.fishery_lut}.",
        "i" = "Check spelling. See {.fn fetch_fishery_names} for valid names."
      )
    )
  }

  result <- result |>
    dplyr::arrange(fishery_name) |>
    dplyr::relocate("year", .after = "fishery_name")

  return(result)
}

#' Get 'fishery manager' table
#'
#' @description Simple wrapper to query the vw_fishery_manager table.
#' @param conn A valid database connection from `connect_creel_db()`
#' @param fishery_name Optional character string for pattern matching in analysis_name
#' @return Tibble of fishery information that includes fishery name, dates, and spatial structure (sections and sites).
#' @export
fishery_manager <- function(
    conn = NULL,
    fishery_name = NULL
  ) {

  # Establish lazy connection if conn not provided
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  filter <- NULL
  if (!is.null(fishery_name)) {
    filter <- glue::glue("fishery_name == '{fishery_name}'")
  }

  fetch_db_table(
    conn,
    schema = "creel",
    table = "vw_fishery_manager",
    filter = filter
  )
}

#' Get 'fishery_catch_groups' view
#' @description Simple wrapper to query the vw_fishery_manager table.
#' @family internal_data
#' @param conn A valid database connection from `connect_creel_db()`
#' @param fishery_name Optional character string for pattern matching in analysis_name
#' @param observed_only Logical. If `TRUE`, return only catch with at least
#'   one observed fish in the fishery's interview data. Existence check only,
#'   no counts are returned. Default `FALSE`.
#' @return Tibble of catch groups of interest for a given fishery.
#' @export
fishery_catchgroups <- function(
    conn = NULL,
    fishery_name = NULL,
    observed_only = FALSE
  ) {

  # Establish lazy connection if conn not provided
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  filter <- NULL
  if (!is.null(fishery_name)) {
    filter <- glue::glue("fishery_name == '{fishery_name}'")
  }

  result <- fetch_db_table(
    conn,
    schema = "creel",
    table  = "vw_model_catch_group",
    filter = filter
    ) |>
    dplyr::arrange(.data$combined_catch_group) |>
    dplyr::select(-c("fishery_id", "catch_group_id")) |>
    dplyr::relocate(
      .data$fishery_name, .data$combined_catch_group, .data$model_catch_group_id,
      .data$species, .data$life_stage, .data$fin_mark, .data$fate
    )

  # Restrict to catch groups with observed catch (presence only)
  if (observed_only) {
    if (is.null(fishery_name)) {
      cli::cli_abort("{.arg observed_only = TRUE} requires a {.arg fishery_name}.")
    }
    observed_ids <- .observed_catch_group_ids(conn, fishery_name)
    result <- dplyr::filter(result, .data$model_catch_group_id %in% observed_ids)
  }

  return(result)
}

#' @keywords internal
.observed_catch_group_ids <- function(conn, fishery_name) {
  # decompose catch groups into atomic components
  components <- fetch_db_table(
    conn, schema = "creel", table = "vw_fishery_catch_group",
    filter = glue::glue("fishery_name == '{fishery_name}'")
  ) |>
    dplyr::inner_join(
      fetch_db_table(conn, schema = "creel", table = "model_catch_group"),
      by = "fishery_catch_group_id"
    ) |>
    dplyr::distinct(
      .data$model_catch_group_id,
      .data$species, .data$life_stage, .data$fin_mark, .data$fate
    )

  # base catch groups from interview raw catch data
  obs <- fetch_data(
    conn = conn, fishery_name = fishery_name,
    tables = c("interview", "catch"), data_source = "internal"
  )
  obs_atomic <- obs$catch |>
    dplyr::semi_join(obs$interview, by = "interview_id") |>
    dplyr::distinct(.data$species, .data$life_stage, .data$fin_mark, .data$fate)

  # ids present in catch data (at least 1 fish)
  components |>
    dplyr::semi_join(obs_atomic,
      by = c("species", "life_stage", "fin_mark", "fate")) |>
    dplyr::distinct(.data$model_catch_group_id) |>
    dplyr::pull(.data$model_catch_group_id)
}

#' Get observed catch groups for a fishery
#'
#' @description
#' Filters the catch group reference list returned by [fishery_catchgroups()] to
#' only those catch groups with observed catch in the dataset. Optionally retains
#' unobserved catch groups with a `fish_count` of zero, which can be useful for
#' communicating about expected-but-absent groups.
#'
#' Catch groups in the reference list may represent combined groups using a `|`
#' separator within any component field (e.g., `Steelhead_Adult_AD|UM|UNK_Released`).
#' These are expanded to their atomic equivalents before matching against observed catch.
#'
#' The standard input for `data` is the list object returned by [fetch_dwg()].
#'
#' @family internal_data
#' @param conn A valid database connection from [connect_creel_db()].
#' @param data A list containing creel dataset components, as returned by [fetch_dwg()].
#'   Must include `$catch`, `$interview`, and `$fishery_manager` elements.
#' @param include_zero Logical. If `TRUE`, unobserved catch groups are retained
#'   with `fish_count = 0`. Default `FALSE`.
#'
#' @return A tibble of catch groups from [fishery_catchgroups()] with an appended
#'   `fish_count` column, filtered to observed catch groups unless `include_zero = TRUE`.
#' @export
fishery_catchgroups_obs <- function(conn = NULL, data, include_zero = FALSE) {

  # Establish lazy connection if conn not provided
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  fishery_name <- unique(data$fishery_manager$fishery_name)

  if (length(fishery_name) != 1L) {
    cli::cli_abort("Expected exactly one fishery name in {.arg data}; found {length(fishery_name)}.")
  }

  cg <- fishery_catchgroups(conn, fishery_name = fishery_name)

  if (nrow(cg) == 0) {
    cli_div(theme = list(span.emph = list(color = "red")))
    cli::cli_abort("No catch groups of interest have been defined for {.emph {fishery_name}}.")
  }

  # Expand composite catch group rows (e.g. "AD|UM|UNK") into one row per
  # atomic combination, retaining the original catch_group label for re-aggregation
  cg_expanded <- purrr::pmap_dfr(
    dplyr::select(cg, .data$fishery_name, .data$catch_group, .data$species, .data$life_stage, .data$fin_mark, .data$fate),
    function(fishery_name, catch_group, species, life_stage, fin_mark, fate) {
      expand.grid(
        fishery_name = fishery_name,
        catch_group  = catch_group,
        species      = strsplit(species,    "\\|")[[1]],
        life_stage   = strsplit(life_stage, "\\|")[[1]],
        fin_mark     = strsplit(fin_mark,   "\\|")[[1]],
        fate         = strsplit(fate,        "\\|")[[1]],
        stringsAsFactors = FALSE
      ) |>
        dplyr::mutate(
          catch_group_atomic = paste(species, life_stage, fin_mark, fate, sep = "_")
        )
    }
  )

  # Summarize observed catch from data, joining fishery_name from interview
  obs <- data$catch |>
    dplyr::left_join(
      dplyr::select(data$interview, "interview_id", "fishery_name"),
      by = "interview_id"
    ) |>
    dplyr::group_by(.data$fishery_name, .data$catch_group) |>
    dplyr::summarise(fish_count = sum(.data$fish_count), .groups = "drop")

  # Join observed counts onto expanded reference, then reaggregate to composite label
  obs_by_cg <- cg_expanded |>
    dplyr::left_join(obs, by = c("fishery_name", "catch_group_atomic" = "catch_group")) |>
    dplyr::group_by(.data$fishery_name, .data$catch_group) |>
    dplyr::summarise(fish_count = sum(.data$fish_count, na.rm = TRUE), .groups = "drop")

  result <- cg |>
    dplyr::left_join(obs_by_cg, by = c("fishery_name", "catch_group")) |>
    dplyr::mutate(fish_count = dplyr::coalesce(.data$fish_count, 0L)) # NA fish count to zero

  if (!include_zero) {
    result <- result |> dplyr::filter(.data$fish_count > 0)
  }

  result
}

#' Get analysis lookup table
#'
#' @description Convenience wrapper to query the model_analysis_lut table with optional filtering.
#' @param conn A valid database connection from `connect_creel_db()`
#' @param analysis_id Optional character string for exact analysis_id match
#' @param fishery_name Optional character string for pattern matching in analysis_name
#' @return Tibble of matching records from model_analysis_lut
#' @export
query_analysis_lut <- function(
    conn = NULL,
    analysis_id = NULL,
    fishery_name = NULL
  ) {

  # Establish lazy connection if conn not provided
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  filter <- NULL
  if (!is.null(analysis_id)) {
    filter <- glue::glue("analysis_id == '{analysis_id}'")
  } else if (!is.null(fishery_name)) {
    filter <- glue::glue("stringr::str_detect(analysis_name, '{fishery_name}')")
  }

  fetch_db_table(
    conn,
    schema = "creel",
    table = "model_analysis_lut",
    filter = filter
  )
}

#' Get model estimates
#'
#' @description Fetch model estimates for a given analysis. Defaults to total (aggregated)
#'   estimates. Stratum-level estimates require at least one filter to prevent accidentally
#'   pulling the entire large table.
#' @param conn A valid database connection.
#' @param analysis_id Character string for the analysis_id to retrieve estimates for
#' @param scale Either "total" (default, aggregated) or "stratum" (granular)
#' @param ... Additional filter conditions passed to `fetch_db_table()`
#' @return Tibble of model estimates
#' @export
model_estimates <- function(
    conn = NULL,
    analysis_id = NULL,
    scale = c("total", "stratum"),
    ...
  ) {

  # Establish lazy connection if conn not provided
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <- connect_creel_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  scale <- match.arg(scale)
  table <- glue::glue("model_estimates_{scale}")

  # Safety check for stratum scale
  if (scale == "stratum" && is.null(analysis_id) && length(list(...)) == 0) {
    cli::cli_abort(c(
      "Cannot query {.field model_estimates_stratum} without filters.",
      "i" = "This table is very large. Please provide at least one of: {.arg analysis_id} or additional filters via {.arg ...}"
    ))
  }

  # Build filters
  filters <- NULL
  if (!is.null(analysis_id)) {
    filters <- c(filters, glue::glue("analysis_id == '{analysis_id}'"))
  }

  # Add any additional filters from ...
  extra_filters <- list(...)
  if (length(extra_filters) > 0) {
    filters <- c(filters, unlist(extra_filters))
  }

  # Query - let fetch_db_table handle NULL filter
  fetch_db_table(
    conn,
    schema = "creel",
    table = table,
    filter = filters
  )
}
