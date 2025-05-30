#' Map datagrade ??
#'
#' @family ETL
#' @param params ??
#' @param transformed_bss_data ??
#' @param transformed_pe_data ??
#'
#' @return ??
#' @export

map_data_grade <- function(params, transformed_bss_data=NULL, transformed_pe_data=NULL) {
  #Map data_grade column to every table
  data_grade_lower <- tolower(params$data_grade) #accept capitalization

  if (data_grade_lower == "approved") {

    transformed_bss_data <- purrr::map(transformed_bss_data,
                                       ~{.$data_grade <- rep("Approved", nrow(.)); .})
    transformed_pe_data <- purrr::map(transformed_pe_data,
                                      ~{.$data_grade <- rep("Approved", nrow(.)); .})

  } else if (data_grade_lower == "provisional") {

    transformed_bss_data <- purrr::map(transformed_bss_data,
                                       ~{.$data_grade <- rep("Provisional", nrow(.)); .})
    transformed_pe_data <- purrr::map(transformed_pe_data,
                                      ~{.$data_grade <- rep("Provisional", nrow(.)); .})

  } else {
    stop("Invalid value for data_grade. Use 'approved' or 'provisional'.")
  }

  #return both objects
  return(list(transformed_bss_data, transformed_pe_data))
}
