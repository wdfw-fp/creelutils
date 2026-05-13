# # Test fetch_data function, compare internal and external data source results
#
# fishery1 <- "Skagit fall salmon 2025"
# fishery2 <- "Hoh winter steelhead 2025-26"
# fishery3 <- "Puyallup Carbon salmon 2025"
# fishery4 <- "Snohomish fall salmon 2025"
# fishery5 <- "Humptulips winter steelhead 2025-26"
#
# d1a <- fetch_data(fishery_name = fishery1, data_source = "internal")
# d1b <- fetch_data(fishery_name = fishery1, data_source = "external")
#
# d2a <- fetch_data(fishery_name = fishery2, data_source = "internal")
# d2b <- fetch_data(fishery_name = fishery2, data_source = "external")
#
# d3a <- fetch_data(fishery_name = fishery3, data_source = "internal")
# d3b <- fetch_data(fishery_name = fishery3, data_source = "external")
#
# d4a <- fetch_data(fishery_name = fishery4, data_source = "internal")
d4b <- fetch_data(fishery_name = fishery4, data_source = "external")
#
# d5a <- fetch_data(fishery_name = fishery5, data_source = "internal")
# d5b <- fetch_data(fishery_name = fishery5, data_source = "external")
#
# compare_rowcounts <- function(internal, external) {
#   all_tables <- union(names(internal), names(external))
#
#   purrr::map_dfr(all_tables, function(tbl) {
#     tibble::tibble(
#       table    = tbl,
#       internal = if (!is.null(internal[[tbl]])) nrow(internal[[tbl]]) else NA_integer_,
#       external = if (!is.null(external[[tbl]])) nrow(external[[tbl]]) else NA_integer_
#     )
#   }) |>
#     dplyr::mutate(diff = .data$internal - .data$external)
# }
#
# purrr::map_dfr(
#   list(
#     `skagit fall salmon 2025` = list(d1a, d1b),
#     `hoh winter steelhead 2025-26`= list(d2a, d2b),
#     `puyallup carbon salmon 2025` = list(d3a, d3b)
#     # `snohomish fall salmon 2025` = list(d4a, d4b),
#     # `humptulips winter steelhead 2025-26` = list(d5a, d5b)
#   ),
#   ~ compare_rowcounts(.x[[1]], .x[[2]]),
#   .id = "fishery"
# ) |> print(n = 100)
#
#
# ############
# # column types - verify str() matches across two data sources
#
#
# # Side-by-side column classes for each element
# a <- purrr::map(d1a, \(x) purrr::map_chr(x, class))
