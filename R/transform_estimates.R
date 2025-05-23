#' Transform individual model outputs into a single object
#'
#' @family ETL
#' @param dwg list object containing freshwater creel dataset, returned by `fetch_dwg()`
#' @param transformed_pe_data standardized outputs from the PE model, returned by `process_pe_estimates()`
#' @param transformed_bss_data standardized outputs from the BSS model, returned by `process_bss_estimates()`
#'
#' @return list object containing standardized model estimates
#' @export
#'
transform_estimates <- function(dwg,
                                transformed_pe_data,
                                transformed_bss_data) {
  #Combine PE and BSS standardized objects

  creel_estimates <- list(
    #table 1
    stratum_catch = rbind(transformed_pe_data$pe_stratum_catch, transformed_bss_data$bss_stratum_catch),
    #table 2
    stratum_effort = rbind(transformed_pe_data$pe_stratum_effort, transformed_bss_data$bss_stratum_effort),
    #table 3
    summarized_catch = rbind(transformed_pe_data$pe_summarized_catch, transformed_bss_data$bss_summarized_catch),
    #table 4
    summarized_effort = rbind(transformed_pe_data$pe_summarized_effort, transformed_bss_data$bss_summarized_effort)
  )

  #combine catch and effort data
  creel_estimates$stratum <- rbind(creel_estimates$stratum_catch, creel_estimates$stratum_effort)
  creel_estimates$total <- rbind(creel_estimates$summarized_catch, creel_estimates$summarized_effort)

  #bring in fishery manager table
  dwg$fishery_manager$catch_area_code <- as.character(dwg$fishery_manager$catch_area_code)

  fishery_manager_trim <- dwg$fishery_manager |>
    dplyr::filter(.data$location_type == "Section") |>
    dplyr::select("section_num", "catch_area_code", "catch_area_description") |>
    dplyr::arrange(.data$section_num) |>
    dplyr::distinct() #assumes no duplicate section_num in fishery manager table

  #join CRC from fishery manager table to estimate tables
  creel_estimates$stratum <- dplyr::left_join(creel_estimates$stratum,
                                              fishery_manager_trim, by ="section_num")

  # fishery_manager_total <- as.vector(fishery_manager_trim$catch_area_code)
  # fishery_manager_total <- paste(fishery_manager_total, collapse = ",")
  # #join CRCs from all sections for total strata
  # creel_estimates$total <- creel_estimates$total |> dplyr::mutate(catch_area_code = fishery_manager_total)

  #rename value column to estimate_value
  creel_estimates$stratum <- creel_estimates$stratum |>
    dplyr::rename("estimate_value" = "value")
  creel_estimates$total <- creel_estimates$total |>
    dplyr::rename("estimate_value" = "value")

  # change angler_final capitalization to match creel database lut
  creel_estimates$stratum <- creel_estimates$stratum |>
    dplyr::mutate(angler_final = dplyr::case_when(
      .data$angler_final == "bank" ~ "Bank",
      .data$angler_final == "boat" ~ "Boat"
    ))

  # add period_timestep field to denote yaml model parameters
  creel_estimates$stratum <- creel_estimates$stratum |>
    dplyr::mutate(period_timestep = dplyr::case_when(
      .data$model_type == "PE" ~ params$period_pe,
      .data$model_type == "BSS" ~ params$period_bss
    ))

  creel_estimates$total <- creel_estimates$total |>
    dplyr::mutate(period_timestep = dplyr::case_when(
      .data$model_type == "PE" ~ params$period_pe,
      .data$model_type == "BSS" ~ params$period_bss
    ))

  ########################################################################################
  ### Performing standardization procedures. This cleanup is needed to better align
  ### different model outputs and in preparation for the development of a data dictionary.
  ### These edits are a post-hoc approach and we plan to implement these changes earlier
  ### in the pipeline during future development phases. - CH 10/9/2024
  ########################################################################################

  tables <- c(5,6) # stratum and total tables location in creel_estimates list

  creel_estimates <- creel_estimates |>
    purrr::map_at(tables, ~.x |>
                    #Modify values within fields
                    dplyr::mutate(
                      #Make BSS outputs match PE
                      estimate_category = dplyr::case_when(
                        #stratum
                        .data$estimate_category == "C_daily" ~ "catch",
                        .data$estimate_category == "E_daily" ~ "effort",
                        .data$estimate_category == "CPUE_daily" ~ "CPUE",
                        #total
                        .data$estimate_category == "C_sum" ~ "catch",
                        .data$estimate_category == "E_sum" ~ "effort",
                        TRUE ~ .data$estimate_category
                      ),
                      #apply snake_case to estimate_type values
                      estimate_type = dplyr::case_when(
                        # estimate_type == "totalobs" ~ "total_observations", ### consider moving to analysis_lut
                        .data$estimate_type == "N_days_open" ~ "total_days_open", ### consider moving to analysis_lut
                        # estimate_type == "totaldaysopen" ~ "total_days_open", ### consider moving to analysis_lut
                        .data$estimate_type == "n_obs" ~ "number_observations",
                        .data$estimate_type == "Rhat" ~ "R_hat",
                        # estimate_type == "n_div" ~ "number_divisions",
                        #estimate_type == "n_eff" ~ "number_draws", #https://mc-stan.org/docs/cmdstan-guide/stansummary.html
                        .data$estimate_type == "df" ~ "degrees_freedom",
                        .data$estimate_type == "sd" ~ "standard_deviation",
                        .data$estimate_type == "se_mean" ~ "standard_error_mean",
                        .data$estimate_type == "est" ~ "estimate_stratum", #applies to catch & effort, which are identified by model_type field
                        .data$estimate_type == "est_sum" ~ "estimate_sum",
                        .data$estimate_type == "catch_est_mean" ~ "catch_estimate_mean",
                        .data$estimate_type == "catch_est_var" ~ "catch_estimate_variance",
                        .data$estimate_type == "ang_hrs_mean" ~ "angler_hours_mean", #or mean_angler_hours ?
                        .data$estimate_type == "ang_hrs_var" ~ "angler_hours_variance",
                        .data$estimate_type == "2.5_pct" ~ "quantile_lower_2_5",
                        .data$estimate_type == "25_pct" ~ "quantile_lower_25",
                        .data$estimate_type == "50_pct" ~ "quantile_median_50",
                        .data$estimate_type == "75_pct" ~ "quantile_upper_75",
                        .data$estimate_type == "97.5_pct" ~ "quantile_upper_97_5",
                        TRUE ~ .data$estimate_type
                      )
                    )
    )

  #Modify fields
  creel_estimates$stratum <- creel_estimates$stratum |>
    dplyr::rename(estimate_time_period = .data$period_timestep)

  creel_estimates$total <- creel_estimates$total |>
    dplyr::rename(estimate_time_period = .data$period_timestep)


  ####################################################################################

  cat("\nTransformed output object 'creel_estimates' created.")

  return(creel_estimates)

}
