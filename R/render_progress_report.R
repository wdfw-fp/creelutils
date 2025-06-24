#' Produce a creel progress report and summaries
#'
#' @description Produces a Creel Progress Report for a given fishery from a standardized template script. These reports are primarily used for in-season monitoring of freshwater recreational fisheries and contains high level summaries of sampling data with preliminary estimates of total catch and effort.
#' @param fishery_name Identifier which represents the spatiotemporal configuration for a given dataset with associated fishery closures.
#' @param bss_used Logical TRUE or FALSE that denotes whether estimates were produced by the Bayesian state-space model. The default option is FALSE.
#' @export
#' @examples
#' \dontrun{
#' render_progress_report("Skykomish summer Chinook and gamefish 2025")
#' render_progress_report("Chehalis winter steelhead 2024-25", bss_used = TRUE)
#' }

render_progress_report <- function(
    fishery_name,
    bss_used = FALSE
) {
  # input validation
  ## CreelEstimates RStudio project
  proj_root <- rprojroot::find_rstudio_root_file()

  if (basename(proj_root) != "CreelEstimates") {
    cli::cli_abort("This function must be run from within the 'CreelEstimates' RStudio project.")
  }

  ## define progress report path
  pr_path <- file.path(proj_root, "template_scripts", "progress_report.qmd")

  ## validate pr_path
  if (!file.exists(pr_path)) {
    cli::cli_abort(glue::glue("Expected file not found: {pr_path}"))
  }

  ## validate fishery_name agrument
  if (is.null(fishery_name) || !is.character(fishery_name) || length(fishery_name) != 1 || fishery_name == "") {
    cli::cli_abort("{.var fishery_name} must be a non-empty string.")
  }

  # validate bss_used argument
  if (!is.logical(bss_used) || length(bss_used) != 1 || is.na(bss_used)) {
    cli::cli_abort("{.var bss_used} must be a single TRUE or FALSE.")
  }

  # construct supplied params list
  params <- list(
    fishery_name = fishery_name,
    bss_used = bss_used
  )
  # render document
  quarto::quarto_render(
    input = pr_path,
    execute_params = params)
}
