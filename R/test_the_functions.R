## test the functions
## Run interactively only — not sourced during package load

# Install from your branch
# devtools::install_github("wdfw-fp/creelutils", ref = "fetch-and-tidy-estimates")

if (interactive()) {
  library(creelutils)
  library(tidyverse)

  fishery_estimates <- get_fishery_estimates(fishery_names = "Nisqually salmon", years = 2022:2023)

  tidy_estimates <- tidy_fishery_estimates(fishery_estimates)

  catch <- tidy_estimates$catch_total |> filter(!project_name == "District 13") |> arrange(fishery_name, catch_group)
}
