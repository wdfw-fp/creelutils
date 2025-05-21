#' Plot angler ZIP codes
#'
#' @param data list, creel dataset where interviewed anglers provided their home zip codes
#' @param type categorical, options: state, US, or full
#' @param unlocated logical, prints a list of zip codes that were not geolocated correctly
#' @import dplyr
#' @import cli
#' @import ggplot2
#' @importFrom tidyr separate_rows tibble
#' @importFrom stringr str_detect
#' @importFrom zipcodeR geocode_zip
#' @importFrom usmap usmap_transform plot_usmap
#' @importFrom sf st_coordinates
#' @importFrom glue glue
#' @importFrom cowplot plot_grid

#' @return plot
#' @export
plot_zipcodes <- function(data, type) {

  #validate input types
  type <- tolower(trimws(type))
  types <- c("wa", "washington", "us", "usa", "full"," both", "all")

  if (!type  %in% types) {
    stop("Incorrect 'type' argument.")
  }

  # Process zip code data ####

  #extract and tidy zip code data
  zipcode <- as_tibble(data$interview$zip_code) |>
    rename(zipcode = value) |>
    filter(!is.na(zipcode)) |>
    separate_rows(zipcode, sep = ",") |> #create a new row for any that have >1 zip code listed
    mutate(zipcode = trimws(zipcode)) |>
    filter(str_detect(zipcode, "^[0-9]{5}$")) #5 digits of 0-9 = a zip code format

  #calculate frequency of each zip code
  zipcodes_freq <- data.frame(table(zipcode))

  #relate zip codes to centroid coordinates
  geocoded <- geocode_zip(unlist(zipcode)) |>
    rename(geo_lat = lat,
           geo_long = lng)

  #initialize zip code database from zipcodeR package
  zip_code_db <- zipcodeR::zip_code_db

  #formatted table of zipcodes and other info
  zipcodes_table <- zipcodes_freq |>
    left_join(geocoded, by = "zipcode") |> #join frequencies with geolocated zips
    left_join(zip_code_db, by="zipcode") |> #join with zipcode database
    select(zipcode, Freq, geo_lat, geo_long, major_city, county, state) |>
    relocate(geo_long, geo_lat) # usmapp::usmap_transform() reqs first two cols be long and lat

  ## handle zips that were not geolocated ####
  unlocated <- zipcodes_table |>
    filter(is.na(geo_lat) | is.na(geo_long)) |>
    select(zipcode, Freq) |>
    arrange(desc(Freq)) |>
    rename(count = Freq) |>
    as_tibble()

  if (length(unlocated) > 0) {
    cli_h1("Warning")
    cli_alert_warning("The following zip codes could not be geolocated:")
    cat("\n")
    print(unlocated)
  } else {
    cli_alert_success("All ZIP codes were successfully geolocated.")
  }

  ## Perform transformation ####

  # moves AK and HI near the western coast
  zipcodes_table <- zipcodes_table |>
    filter(!is.na(geo_lat) | !is.na(geo_long)) |>   #filter unlocated zipsnow that they have been isolated
    usmap_transform(input_names = c("geo_long", "geo_lat"))

  #convey geometry to point pairs
  geometry <- st_coordinates(zipcodes_table)
  zipcodes_table <- cbind(zipcodes_table, geometry)

  # define plots ####

  ## legend breaks ####

  #calculate logarithmic breaks, good for skewed data
  values <- zipcodes_table$Freq

  my_breaks <- c(1, 10, 50, 100, 200)
  my_labels <- format(my_breaks, big.mark = ",")

  # my_breaks <- round(exp(seq(log(min(values[values > 0], na.rm = TRUE)),
  #                      log(max(values, na.rm = TRUE)), length.out = 5)))
  # my_labels <- format(my_breaks, big.mark = ",")

  # log_values <- log10(values[values > 0])
  # log_breaks <- pretty(log_values, n = 5)
  # my_breaks <- round(10^log_breaks)
  # my_labels <- format(my_breaks, big.mark = ",")

  ## custom theme ####
  my_theme <- function(my_breaks, my_labels) {
    list(
      # Size
      scale_size_continuous(
        name = "Frequency",
        trans = "log",
        breaks = my_breaks,
        labels = my_labels,
        range = c(2, 10)
      ),
      # Color
      scale_color_viridis(
        name = "Frequency",
        guide = "legend",
        trans = "log",
        breaks = my_breaks,
        labels = my_labels,
        option = "inferno"
      ),
      # Opacity
      scale_alpha_continuous(
        name = "Frequency",
        guide = "legend",
        trans = "log",
        breaks = my_breaks,
        labels = my_labels,
        range = c(0.3, 0.9)
      ),
      # Theme
      theme(
        text = element_text(size = 12, color = "#22211d"),
        # plot.background = element_rect(fill = "#F8F8F9", color = NA),
        # panel.background = element_rect(fill = "#F8F8F9", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "white", color = NA)
      )
    )
  }

  # Filter by region
  if (type %in% c("wa", "washington")) {
    zip_plot_data <- zipcodes_table |> filter(state == "WA")
    base_map <- plot_usmap(regions = "counties", include = "WA", fill = "#92D4DA", alpha = 0.7)
  } else {
    zip_plot_data <- zipcodes_table
    base_map <- plot_usmap(regions = "states", fill = "#92D4DA", alpha = 0.7)
  }

  # Final plot
  map_plot <- base_map +
    geom_point(
      data = zip_plot_data,
      aes(x = X, y = Y, size = Freq, color = Freq, alpha = Freq)
    ) +
    my_theme(my_breaks, my_labels)

  return(map_plot)
}
