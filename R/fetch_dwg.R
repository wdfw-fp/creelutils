#' Download creel datasets from data.wa.gov
#'
#' Download freshwater recreational fishery creel datasets from Washington's public data warehouse, data.wa.gov. When a valid 'fishery_name' is provided several database views are downloaded (i.e., effort, interview, catch, water bodies, closures, and a )
#'
#' @param fishery_name Identifier which represents the spatiotemporal configuration for a given dataset with associated fishery closures.
#'
#' @importFrom rlang .data
#' @importFrom utils URLencode
#' @importFrom tidyr drop_na
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate
#' @importFrom purrr map_int
#' @importFrom tibble tibble
#' @importFrom cli cli_abort
#'
#' @return dwg, a list object containing a dataframe for each database view downloaded (e.g., effort, interview, catch)
#' @export
#'
#' @examples
#' \dontrun{
#' dwg <- fetch_dwg("Skagit winter steelhead 2021")
#' }
fetch_dwg <- function(fishery_name){

  #links to database views
  dwg_base <- list(
    #event = "https://data.wa.gov/resource/ui95-axtn.csv",
    effort = "https://data.wa.gov/resource/h9a6-g38s.csv",
    interview = "https://data.wa.gov/resource/rpax-ahqm.csv",
    catch = "https://data.wa.gov/resource/6y4e-8ftk.csv",
    water_bodies = "https://data.wa.gov/resource/nbd2-vdmz.csv",
    closures = "https://data.wa.gov/resource/6zm6-iep6.csv",
    #,gear = "https://data.wa.gov/resource/d2ks-afhz.csv" #currently unused?
    fishery_manager = "https://data.wa.gov/resource/vkjc-s5u8.csv"
  )

  #initialize list to store data
  dwg <- list()

  #effort data
  dwg$effort <- paste0(
    dwg_base$effort,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    tidyr::drop_na(.data$count_type) |>
    dplyr::select(-.data$created_datetime, -.data$modified_datetime)

  # ll = latitude and longitude coordinates for water bodies
  dwg$ll <- paste0(
    dwg_base$water_bodies,
    "?$where=water_body_desc in('",
    paste0(unique(dwg$effort$water_body), collapse = "','"),
    "')&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F)

  # interview data
  dwg$interview <- paste0(
    dwg_base$interview,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select(
      -.data$created_datetime, -.data$modified_datetime)

  # catch data
  dwg$catch <- paste0(
    dwg_base$catch,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select(.data$interview_id, .data$catch_id, .data$species, .data$run, .data$life_stage, .data$fin_mark, .data$sex, .data$fork_length_cm, .data$fate, .data$fish_count) |>
    dplyr::mutate(
      catch_group = paste(.data$species, .data$life_stage, .data$fin_mark, .data$fate, sep = "_") # fish catch groups to estimate catch of
    )

  # fishery closure data
  dwg$closures <- paste0(
    dwg_base$closures,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select(.data$fishery_name, .data$section_num, .data$event_date)

  dwg$fishery_manager <- paste0(
    dwg_base$fishery_manager,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F)

  # create summary table to show amount of data downloaded
  summary_table <- tibble::tibble(
    `Data Component` = names(dwg),
    Records = purrr::map_int(dwg, nrow)
  )

  if(all(summary_table$Records == 0)) {
    cli::cli_abort("fetch_dwg error: No data downloaded. Check that the 'fishery_name' is valid.")
  }

  print(summary_table)

  return(dwg)
}
