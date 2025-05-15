#' Summarize priors, sampler settings, and metadata from a stanfit object
#'
#' @param ecg Estimate catch group (e.g., Steelhead_Adult_UM_Released).
#' @param ecg_fit A fitted model (stanfit object)
#' @param inputs_bss Inputs of the stan model
#' @param print Logical. Return summary to Console
#'
#' @return List with model priors, sampler settings, and metadata
get_bss_settings <- function(ecg, ecg_fit, inputs_bss, print = FALSE) {

  # --- Priors
  prior_values <- inputs_bss[[ecg]][grep("^value_", names(inputs_bss[[ecg]]))]

  priors_tbl <- tibble(
    catch_group = ecg,
    prior_name = names(prior_values),
    prior_value = unlist(prior_values)
  )

  # --- Sampler settings from first chain
  chain_args <- ecg_fit@stan_args[[1]]

  sampler_settings <- tibble(
    catch_group = ecg,
    n_chains = length(ecg_fit@stan_args),
    iter = chain_args$iter,
    warmup = chain_args$warmup,
    adapt_delta = chain_args$control$adapt_delta,
    max_treedepth = chain_args$control$max_treedepth,
    thin = chain_args$thin,
    init = chain_args$init,
    seed = chain_args$seed
  )

  # --- Metadata
  model_metadata <- tibble(
    catch_group = ecg,
    date = ecg_fit@date,
    model_name = ecg_fit@model_name,
    mode = ecg_fit@mode,
    algorithm = chain_args$algorithm,
    method = chain_args$method
  )

  priors_tbl_sub <- priors_tbl |> select(-.data$catch_group)

  if (print) {
    cat("\nBSS model settings summary\n")
    cat("\n--- Priors ---\n")
    print(priors_tbl_sub)
    cat("\n--- Sampler Settings ---\n")
    print(sampler_settings)
    cat("\n--- Model Metadata ---\n")
    print(model_metadata)
  }

  return(list(
    priors = priors_tbl,
    sampler_settings = sampler_settings,
    metadata = model_metadata
  ))
}
