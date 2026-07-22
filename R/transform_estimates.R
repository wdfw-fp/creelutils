#' Transform individual model outputs into a single object
#'
#' @family ETL
#' @param params list object containing fishery- and analysis- specific metadata
#' @param transformed_pe_data standardized outputs from the PE model, returned by `process_pe_estimates()`
#' @param transformed_bss_data standardized outputs from the BSS model, returned by `process_bss_estimates()`
#'
#' @return list object containing standardized model estimates
#' @export
transform_estimates <- function(
    params,
    transformed_pe_data,
    transformed_bss_data
) {
  # Combine PE and BSS standardized outputs into the two final tables
  # Grain is carried by the table (stratum vs total)
  # catch/effort by estimate_category and  model by model_type
  creel_estimates <- list(
    stratum = rbind(
      transformed_pe_data$pe_stratum_catch,
      transformed_bss_data$bss_stratum_catch,
      transformed_pe_data$pe_stratum_effort,
      transformed_bss_data$bss_stratum_effort
    ),
    total = rbind(
      transformed_pe_data$pe_summarized_catch,
      transformed_bss_data$bss_summarized_catch,
      transformed_pe_data$pe_summarized_effort,
      transformed_bss_data$bss_summarized_effort
    )
  )
  #rename value column to estimate_value
  creel_estimates$stratum <- creel_estimates$stratum |> dplyr::rename("estimate_value" = "value")
  creel_estimates$total <- creel_estimates$total |> dplyr::rename("estimate_value" = "value")

  # change angler_final capitalization to match creel database lut
  creel_estimates$stratum <- creel_estimates$stratum |>
    dplyr::mutate(angler_final = dplyr::case_when(
      .data$angler_final == "bank" ~ "Bank",
      .data$angler_final == "boat" ~ "Boat"
    ))

  # add period_timestep field to denote yaml model parameters
  creel_estimates$stratum <- creel_estimates$stratum |>
    dplyr::mutate(estimate_time_period = dplyr::case_when(
      .data$model_type == "PE" ~ params$period_pe,
      .data$model_type == "BSS" ~ params$period_bss
    ))

  creel_estimates$total <- creel_estimates$total |>
    dplyr::mutate(estimate_time_period = dplyr::case_when(
      .data$model_type == "PE" ~ params$period_pe,
      .data$model_type == "BSS" ~ params$period_bss
    ))

  # Performing standardization procedures
  creel_estimates <- creel_estimates |>
    purrr::map(~.x |>
      #Modify values within fields
      dplyr::mutate(
        # Estimate category ---------------------------------------------------------------------
        estimate_category = dplyr::case_when(
          .data$estimate_category %in% c("C_daily", "C_sum") ~ "catch",
          .data$estimate_category %in% c("E_daily", "E_sum") ~ "effort",
          .data$estimate_category == "CPUE_daily" ~ "cpue",
          TRUE ~ .data$estimate_category
        ),
        # Estimate type --------------------------------------------------------------------------
        estimate_type = dplyr::case_when(
          # point estimate — collapse across model_type and grain (table denotes stratum vs total)
          # PE est / PE est_sum / BSS posterior mean
          .data$estimate_type %in% c("est", "est_sum", "mean") ~ "estimate",

          # PE per-day component moments (pre-expansion; estimate = daily_mean * days_open)
          .data$estimate_type %in% c("catch_est_mean", "ang_hrs_mean") ~ "daily_mean",
          .data$estimate_type %in% c("catch_est_var", "ang_hrs_var") ~ "daily_variance",

          # survey coverage metadata
          .data$estimate_type %in% c("n_obs", "totalobs") ~ "number_observations",
          .data$estimate_type %in% c("N_days_open", "totaldaysopen") ~ "days_open",

          # BSS dispersion
          .data$estimate_type == "sd"      ~ "standard_deviation",
          .data$estimate_type == "se_mean" ~ "standard_error",

          # BSS posterior quantiles
          .data$estimate_type == "2.5_pct" ~ "quantile_2_5",
          .data$estimate_type == "25_pct" ~ "quantile_25",
          .data$estimate_type == "50_pct" ~ "quantile_50",
          .data$estimate_type == "75_pct" ~ "quantile_75",
          .data$estimate_type == "97.5_pct" ~ "quantile_97_5",

          # diagnostics
          .data$estimate_type == "Rhat" ~ "r_hat",
          .data$estimate_type == "n_eff" ~ "n_eff",
          .data$estimate_type == "n_div" ~ "n_div",
          .data$estimate_type == "df" ~ "degrees_freedom",
          TRUE ~ .data$estimate_type
        )
      )
    )

  # Reduce estimate types sent to total table, stratum retains all
  # drop days_open, number_observations, r_hat, n_eff, and n_div
  creel_estimates$total <- creel_estimates$total |>
    dplyr::filter(.data$estimate_type %in% c(
      "estimate", "quantile_2_5", "quantile_25", "quantile_50", "quantile_75", "quantile_97_5"
    ))

  cli::cli_alert_success("Transformed output object {.val creel_estimates} created.")

  return(creel_estimates)
}
