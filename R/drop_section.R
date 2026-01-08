#' Drop sections from creel survey data
#'
#' Removes specified sections from all components of a creel survey dataset,
#' including interview, effort, catch, and days tables.
#'
#' @param data A list containing creel survey data with required tables:
#'   `interview`, `effort`, `catch`, `days`, and `fishery_manager`.
#' @param section Numeric vector of section numbers to drop. Invalid
#'   section numbers are ignored with a warning. Must leave at least one section
#'   remaining.
#'
#' @return A modified list with the same structure as `data`, with specified
#'   sections removed from all applicable tables. Also removes corresponding
#'   `open_section_X` columns from the `days` table.
#'
#' @examples
#' \dontrun{
#' # Drop a single section
#' data_filtered <- drop_section(data, section = 3)
#'
#' # Drop multiple sections
#' data_filtered <- drop_section(data, section = c(1, 2))
#' }
#'
#' @export
drop_section <- function(data, section) {

  # Validate inputs
  if (!is.list(data)) cli::cli_abort("'data' must be a list object")

  required_tables <- c("interview", "effort", "catch", "days", "fishery_manager")
  missing_tables <- setdiff(required_tables, names(data))
  if (length(missing_tables) > 0) {
    cli::cli_abort("Missing required tables: {.field {missing_tables}}")
  }

  if (!is.numeric(section) && !is.integer(section)) {
    cli::cli_abort("'section' must be numeric or integer")
  }

  # Get available sections
  available_sections <- data$fishery_manager$section_num |> unique() |> sort()

  # Validate sections to drop
  invalid_sections <- setdiff(section, available_sections)
  if (length(invalid_sections) > 0) {
    cli::cli_warn("Ignoring non-existent section{?s}: {.val {invalid_sections}}")
  }

  sections_to_drop <- intersect(section, available_sections)
  remaining_sections <- setdiff(available_sections, sections_to_drop)

  if (length(remaining_sections) == 0) {
    cli::cli_abort("Cannot drop all sections. At least one must remain.")
  }

  if (length(sections_to_drop) == 0) {
    cli::cli_alert_info("No valid sections to drop.")
    return(data)
  }

  cli::cli_alert_success("Dropping section{?s}: {paste(sections_to_drop, collapse = ', ')}")

  # Filter tables
  data$interview <- data$interview |> filter(!section_num %in% sections_to_drop)
  data$effort <- data$effort |> filter(!section_num %in% sections_to_drop)

  # Filter catch by valid interview_ids
  valid_ids <- data$interview$interview_id
  data$catch <- data$catch |> filter(interview_id %in% valid_ids)

  # Remove open_section columns from days table
  section_cols <- paste0("open_section_", sections_to_drop)
  data$days <- data$days |> select(-any_of(section_cols))

  # Filter closures if present
  if ("closures" %in% names(data) && "section_num" %in% names(data$closures)) {
    data$closures <- data$closures |> filter(!section_num %in% sections_to_drop)
  }

  cli::cli_alert_info(" Remaining section{?s}: {paste(remaining_sections, collapse = ', ')}")

  return(data)
}
