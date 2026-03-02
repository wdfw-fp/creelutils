# Download the Skagit winter steelhead 2021 fishery dataset
# Sourced from Washington State's Open Data Portal, data.wa.gov

example_data <- creelutils::fetch_dwg("Skagit winter steelhead 2021", print = FALSE)

usethis::use_data(example_data, overwrite = TRUE)
