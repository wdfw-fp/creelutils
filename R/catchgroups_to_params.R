#' Format catch groups as YAML param strings
#'
#' @description
#' Adds a `param_string` column to a catch groups tibble, formatting each row
#' as a `c(species = '...', life_stage = '...', fin_mark = '...', fate = '...')`
#' string suitable for pasting into the `est_catch_groups` YAML param of a
#' CreelEstimates script.
#'
#' Composite catch groups (e.g. `Adult|Jack`, `AD|UM|UNK`) are preserved
#' literally — one row in equals one `param_string` out.
#'
#' Typical workflow: pipe the output of [fishery_catchgroups_obs()] through
#' this function with `print = TRUE` to emit the comma-separated rows to the
#' console, then paste between the `rbind(` and `))` of the script's
#' `est_catch_groups` YAML param.
#'
#' @param data A tibble with `species`, `life_stage`, `fin_mark`, and `fate`
#'   columns, typically the output of [fishery_catchgroups_obs()] or
#'   [fishery_catchgroups()].
#' @param print Logical. If `TRUE`, prints `param_string` values to the console
#'   separated by `,\n` (ready to paste into a YAML param) and returns the
#'   tibble invisibly. Default `FALSE`.
#'
#' @return The input tibble with an appended `param_string` character column.
#'   Returned invisibly when `print = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_creel_db()
#' dwg <- fetch_dwg("Skagit fall salmon 2025")
#'
#' # Print to console for copy-paste into est_catch_groups
#' fishery_catchgroups_obs(con, dwg) |>
#'   catchgroups_to_params(print = TRUE)
#' }
catchgroups_to_params <- function(data, print = FALSE) {

  required_cols <- c("species", "life_stage", "fin_mark", "fate")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg data} is missing required column{?s}:",
      "x" = "{.field {missing_cols}}"
    ))
  }

  result <- data |>
    dplyr::mutate(
      param_string = glue::glue(
        "c(species = '{species}', life_stage = '{life_stage}', ",
        "fin_mark = '{fin_mark}', fate = '{fate}')"
      ) |>
        as.character()
    )

  if (print) {
    cat(result$param_string, sep = ",\n")
    cat("\n")
    return(invisible(result))
  }

  result
}
