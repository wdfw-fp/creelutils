#' List all available 'fishery name' values
#'
#' @family public_data
#' @param ... .
#' @return Character vector of "fishery_name" values which represent identifiers for a given dataset, which is comprised of multiple elements (e.g., effort, interview, catch, etc.).
#' @export
#'
#' @examples
#' head(fetch_fishery_names(), n = 10)
fetch_fishery_names = function(...){
  if (length(list(...)) > 0) {
    cli::cli_abort("The function {.fn fetch_fishery_names} does not take any arguments. Call it as {.code fetch_fishery_names()} with empty parentheses.")
  }

  dat <- readr::read_csv(utils::URLencode("https://data.wa.gov/resource/vkjc-s5u8.csv?$limit=200000"),
                         show_col_types = F)
  fisheries <- unique(dat[["fishery_name"]]) |>
    as.character() |>
    stats::na.omit() |>
    sort()

  return(fisheries)
}

#' Find fishery names from a partial names
#'
#' Helps identify options for `fetch_dwg()`.
#'
#' @family public_data
#' @param fishery_partial Partial fishery name. Can take regular expressions, ignores capitalization.
#' @return Character vector of 'fishery_name' identifiers containing the partial value.
#' @export
#'
#' @examples
#' search_fishery_name("gamefish")
#' search_fishery_name("Humptulips")
search_fishery_name <- function(fishery_partial){
  grep(fishery_partial, fetch_fishery_names(), ignore.case = T, value = TRUE)
}
