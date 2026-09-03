#' Write metadata to analysis lookup table
#' @param conn database connection
#' @param analysis_lut analysis lookup table
write_lut <- function(conn, analysis_lut) {
  DBI::dbAppendTable(
    conn = conn,
    name = DBI::Id(schema = "creel", table = "model_analysis_lut"),
    value = analysis_lut
  )
}

#' Write estimates to total estimates table
#' @param conn database connection
#' @param total summarized season-total estimates
write_total <- function(conn, total) {
  DBI::dbAppendTable(
    conn = conn,
    name = DBI::Id(schema = "creel", table = "model_estimates_total"),
    value = total
  )
}

#' Write estimates to stratum estimates table
#' @param conn database connection
#' @param stratum fine grain estimates
write_stratum <- function(conn, stratum) {
  DBI::dbAppendTable(
    conn = conn,
    name = DBI::Id(schema = "creel", table = "model_estimates_stratum"),
    value = stratum
  )
}
