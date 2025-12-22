#' Get fishery lookup table
#'
#' @description Simple wrapper to query the fishery_lut table.
#' @param conn A valid database connection from `establish_db_con()`
#'
#' @return Tibble of fishery names with year, start dates, end dates, and metadata
#' @export
fishery_lut <- function(conn) {

  # Check database connection
  if (!DBI::dbIsValid(conn)) {
    cli::cli_abort("Database connection is not valid or has been closed.")
  }

  fetch_db_table(conn, "creel", "fishery_lut")

}

#' Get 'fishery manager' table
#'
#' @description Simple wrapper to query the vw_fishery_manager table.
#' @param conn A valid database connection from `establish_db_con()`
#' @param fishery_name Optional character string for pattern matching in analysis_name
#' @return Tibble of fishery information that includes fishery name, dates, and spatial structure (sections and sites).
#' @export
fishery_manager <- function(
    conn,
    fishery_name = NULL
  ) {

  # Check database connection
  if (!DBI::dbIsValid(conn)) {
    cli::cli_abort("Database connection is not valid or has been closed.")
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
#' @param conn A valid database connection from `establish_db_con()`
#' @param fishery_name Optional character string for pattern matching in analysis_name
#' @return Tibble of catch groups of interest for a given fishery.
#' @export
fishery_catchgroups <- function(
    conn,
    fishery_name = NULL
  ) {

  # Check database connection
  if (!DBI::dbIsValid(conn)) {
    cli::cli_abort("Database connection is not valid or has been closed.")
  }

  filter <- NULL
  if (!is.null(fishery_name)) {
    filter <- glue::glue("fishery_name == '{fishery_name}'")
  }

  fetch_db_table(
    conn,
    schema = "creel",
    table  = "vw_fishery_catch_group",
    filter = filter
  ) |>
    dplyr::select(-dplyr::contains("_id")) |>
    dplyr::relocate(fishery_name, species, life_stage, fin_mark, fate) |>
    dplyr::mutate(catch_group = paste(species, life_stage, fin_mark, fate, sep = "_"))
}

#' Get analysis lookup table
#'
#' @description Convenience wrapper to query the model_analysis_lut table with optional filtering.
#' @param conn A valid database connection from `establish_db_con()`
#' @param analysis_id Optional character string for exact analysis_id match
#' @param fishery_name Optional character string for pattern matching in analysis_name
#' @return Tibble of matching records from model_analysis_lut
#' @export
analysis_lut <- function(
    conn = NULL,
    analysis_id = NULL,
    fishery_name = NULL
  ) {

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
