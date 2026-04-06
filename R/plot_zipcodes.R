# cbind() and table() in plot_zipcodes() produce these vars, cols from baase operations
# defining as a global var prevents a warning in R-CMD-Check
utils::globalVariables(c("X", "Y", "Freq"))

#' Plot angler ZIP codes
#'
#' Creates a map displaying the home ZIP codes reported by anglers during creel
#' interviews, conveying the geographic range and density of angler origin for a
#' given fishery.
#'
#' @param data list, creel dataset where interviewed anglers provided their home
#'   zip codes (expects `data$interview$zip_code`).
#' @param type character, map extent. Either `"wa"` (default) or `"us"`.
#'   Case-insensitive.
#'
#' @importFrom ggplot2 geom_point aes theme element_text element_rect labs
#'   scale_size_continuous scale_color_viridis_c scale_alpha_continuous
#' @importFrom dplyr filter mutate rename select relocate arrange left_join
#' @importFrom rlang .data
#' @importFrom tidyr separate_longer_delim
#' @return A ggplot object
#' @export
plot_zipcodes <- function(data, type = "wa") {

  # packages in DESCRIPTION Suggests
  rlang::check_installed(
    c("scales", "sf", "usmap", "zipcodeR"),
    reason = "to use `plot_zipcodes()`"
  )

  # Validate and normalize type argument ----
  type <- toupper(trimws(type))
  type <- match.arg(type, choices = c("WA", "US"))

  # Process zip code data ----
  raw_zipcodes <- data$interview$zip_code

  # Report ZIP code coverage ----
  n_total    <- nrow(data$interview)
  n_with_zip <- sum(!is.na(data$interview$zip_code))
  pct        <- round(100 * n_with_zip / n_total)
  cli::cli_h1("ZIP code representation: {n_with_zip} of {n_total} interviews ({pct}%)")

  # Silently return NULL and message user if the dataset does not contain ZIP codes
  if (n_with_zip == 0) {
    cli::cli_alert_warning(
      "No ZIP codes were recorded for this fishery. {.fn plot_zipcodes} requires ZIP code data to generate a map."
    )
    return(invisible(NULL))
  }

  # Detect and report Canadian postal codes (alphanumeric, e.g. "V5K 0A1")
  # before filtering to US-only format. Canadian codes are not currently
  # geocoded; this count is informational only.
  canadian_pattern <- "^[A-Za-z][0-9][A-Za-z]\\s?[0-9][A-Za-z][0-9]$"
  canadian_codes <- trimws(unlist(strsplit(as.character(raw_zipcodes[!is.na(raw_zipcodes)]), ",")))
  canadian_codes <- canadian_codes[stringr::str_detect(canadian_codes, canadian_pattern)]

  if (length(canadian_codes) > 0) {
    cli::cli_h3("Note: Canadian postal codes detected (n={length(canadian_codes)}):")
    cli::cli_text("{paste(canadian_codes, collapse = ', ')}")
    cli::cli_alert_info("Canadian postal codes are not currently geocoded.")
  }

  # Extract and tidy US zip codes ----
  zip_tbl <- tibble::tibble(zipcode = as.character(raw_zipcodes)) |>
    filter(!is.na(.data$zipcode)) |>
    separate_longer_delim(.data$zipcode, delim = ",") |>  # one row per zip when >1 listed
    mutate(zipcode = trimws(.data$zipcode)) |>
    filter(stringr::str_detect(.data$zipcode, "^[0-9]{5}$"))  # US zip code format only

  # Frequency of each unique zip code
  zipcodes_freq <- as.data.frame(table(zip_tbl$zipcode)) |>
    rename(zipcode = .data$Var1)

  # Load zip code database and join coordinates and metadata ----
  zip_code_db <- zipcodeR::zip_code_db

  zipcodes_table <- zipcodes_freq |>
    left_join(zip_code_db, by = "zipcode") |>
    select(.data$zipcode, .data$Freq, .data$lat, .data$lng, .data$major_city, .data$county, .data$state) |>
    rename(geo_lat = .data$lat, geo_long = .data$lng) |>
    relocate(.data$geo_long, .data$geo_lat)  # usmap_transform() requires long, lat as first two cols

  # Report unlocated zip codes ----
  unlocated <- zipcodes_table |>
    filter(is.na(.data$geo_lat) | is.na(.data$geo_long)) |>
    select(.data$zipcode, .data$Freq) |>
    arrange(desc(.data$Freq)) |>
    rename(count = .data$Freq) |>
    tibble::as_tibble()

  if (nrow(unlocated) > 0) {
    cli::cli_h3("Warning: The following ZIP codes could not be geolocated (n={nrow(unlocated)}):")
    cli::cli_text("{paste(unlocated$zipcode, collapse = ', ')}")
    cli::cli_alert_info("These ZIP codes are typically either associated with a PO Box or data entry error.")
  } else {
    cli::cli_alert_success("All ZIP codes were successfully geolocated.")
  }

  # Transform coordinates ----
  # Filters unlocated zips and relocates AK/HI near the western coast
  zipcodes_table <- zipcodes_table |>
    filter(!is.na(.data$geo_lat) & !is.na(.data$geo_long)) |>
    usmap::usmap_transform(input_names = c("geo_long", "geo_lat"))

  # Convert sf geometry to plain X/Y columns for ggplot
  geometry <- sf::st_coordinates(zipcodes_table)
  zipcodes_table <- cbind(zipcodes_table, geometry)

  # Data-driven legend breaks ----
  values <- zipcodes_table$Freq

  # scales::breaks_log() computes clean breaks from the actual data range,
  # handling both low- and high-pressure fisheries without hardcoding.
  # n = 5 is a target; the function may return fewer.
  my_breaks <- scales::breaks_log(n = 5)(range(values[values > 0], na.rm = TRUE))
  my_labels <- scales::label_comma()(my_breaks)

  # Custom theme ----
  my_theme <- function(my_breaks, my_labels) {
    list(
      scale_size_continuous(
        name = "Frequency",
        trans = "log",
        breaks = my_breaks,
        labels = my_labels,
        range = c(2, 10)
      ),
      scale_color_viridis_c(
        name = "Frequency",
        guide = "legend",
        trans = "log",
        breaks = my_breaks,
        labels = my_labels,
        option = "inferno"
      ),
      scale_alpha_continuous(
        name = "Frequency",
        guide = "legend",
        trans = "log",
        breaks = my_breaks,
        labels = my_labels,
        range = c(0.3, 0.9)
      ),
      theme(
        text = element_text(size = 12, color = "#22211d"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5)
      )
    )
  }

  # Build map ----
  if (type == "WA") {
    zip_plot_data <- zipcodes_table |> filter(.data$state == "WA")
    base_map <- usmap::plot_usmap(regions = "counties", include = "WA", fill = "#92D4DA", alpha = 0.7)
  } else {
    zip_plot_data <- zipcodes_table
    base_map <- usmap::plot_usmap(regions = "states", fill = "#92D4DA", alpha = 0.7)
  }

  map_plot <- base_map +
    geom_point(
      data = zip_plot_data,
      aes(x = X, y = Y, size = Freq, color = Freq, alpha = Freq)
    ) +
    my_theme(my_breaks, my_labels) +
    labs(title = unique(data$interview$fishery_name))

  return(map_plot)
}
