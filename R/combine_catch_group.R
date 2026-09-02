#' Combine catch group components
#'
#' Accepts a dataframe which must contain four columns ("species", "life_stage",
#' "fin_mark", "fate"). These columns are combined into a single string for each
#' row where the component columns are separated by an underscore "_".
#' `NA` values are forced to 'NA' characters.
#'
#' @param df data frame of catch groups
#'
#' @returns character string of combined catch groups
#' @export
combine_catch_group <- function(df) {
  components <- c("species", "life_stage", "fin_mark", "fate")

  missing <- setdiff(components, names(df))
  if (length(missing)) {
    cli::cli_abort(
      "{.arg df} is missing column{?s} {.field {missing}}."
    )
  }

  do.call(
    paste,
    c(
      lapply(df[components], \(x) tidyr::replace_na(as.character(x), "NA")),
      list(sep = "_")
    )
  )
}
