#' Get raw catch data for a fishery
#'
#' @param fishery Fishery name
#' @param start_date Optional. First date of window.
#' @param end_date Optional. Last date of window.
#' @import dplyr
#' @import cli
#' @importFrom rlang .data
#' @return Table of raw, unexpanded catch reported from angler interviews.
#' @export
raw_catch <- function(fishery, start_date = NULL, end_date= NULL) {

  # Validate that both dates are provided together or both are NULL
  if (xor(is.null(start_date), is.null(end_date))) {
    cli_abort(c(
      "Both date arguments must be provided together",
      "x" = "You provided only one of {.arg start_date} or {.arg end_date}",
      "i" = "Either provide both dates or leave both as {.code NULL}"
    ))
  }

  # Download public data
  dwg <- fetch_dwg(fishery)

  catch <- dwg$catch |> left_join(dwg$interview, by = "interview_id")

  # Apply date filter only if both dates are provided
  if (!is.null(start_date) && !is.null(end_date)) {
    catch <- catch |>
      filter(between(.data$event_date, as.Date(start_date), as.Date(end_date)))
  }

  catch <- catch |>
    group_by(.data$fishery_name, .data$catch_group) |>
    summarise(fish_count = sum(.data$fish_count), .groups = "drop") |>
    select(-.data$fishery_name) |>
    as.data.frame()

  #CLI reporting to console
  cli_div(theme = list(span.emph = list(color = "red")))
  cat_line()
  cli_alert_info("Raw catch reported for the {.emph {fishery}} fishery.")
  cat_line()

  return(catch)
}
