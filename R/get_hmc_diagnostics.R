#' Summarize HMC Diagnostics from a Stan Fit
#'
#' Computes a human-readable summary of key Hamiltonian Monte Carlo (HMC) diagnostics
#' from a fitted Stan model object, including divergences, tree depth saturation, and
#' E-BFMI values across chains.
#'
#' This function reconstructs diagnostics similar to `rstan::check_hmc_diagnostics()` but returns
#' a formatted character string instead of printing to the console. This allows the output to be saved,
#' displayed, or logged programmatically.
#'
#' @param stan_fit A fitted Stan model object of class `stanfit`, typically created using `rstan::stan()` or `rstan::sampling()`.
#'
#' @return A single character string summarizing the following diagnostics:
#' \itemize{
#'   \item Number and percentage of post-warmup iterations with divergences.
#'   \item Number of iterations that saturated the maximum tree depth.
#'   \item E-BFMI values for each chain, indicating potential energy sampling issues.
#' }
#'
#' @details
#' This function summarizes key Hamiltonian Monte Carlo (HMC) convergence diagnostics
#' from a Stan model fit. It is intended to help users assess whether further tuning is
#' needed using available options like `adapt_delta`, `max_treedepth`, or prior adjustments.
#'
#' The diagnostics reported include:
#' \itemize{
#'   \item \strong{Divergent transitions}: Should be less than 5% of post-warmup iterations.
#'         Divergences may indicate poor exploration of the posterior and often require increasing
#'         `adapt_delta` (e.g., from 0.75 up toward 0.95 or 0.99) to reduce the step size. Smaller
#'         steps improve numerical stability but increase runtime.
#'
#'   \item \strong{Maximum tree depth}: No iterations should saturate the maximum tree depth.
#'         Saturation may co-occur with high `adapt_delta` due to long trajectories. If frequent,
#'         consider increasing `max_treedepth` above the default (usually 10–12).
#'
#'   \item \strong{E-BFMI}: This metric checks whether the energy transitions in HMC are efficient.
#'         E-BFMI below 0.2 is a sign of potential sampling issues, though reparameterization
#'         (typically recommended) is not accessible in this workflow. Instead, consider revisiting
#'         prior distributions to improve geometry.
#' }
#'
#' This function is adapted to return results as text, in contrast to `rstan::check_hmc_diagnostics()`
#' which prints directly to the console.
#'
#' @importFrom rstan get_num_divergent get_num_max_treedepth get_bfmi
#' @export

get_hmc_diagnostics <- function(stan_fit) {
  ## rstan::check_hmc_diagnostics() prints to console and cannot be saved out (even w/ capture.output())
  ## reconstructing output here from components

  # Extract the number of chains
  num_chains <- stan_fit@sim$chains

  # Extract the number of iterations and warmup iterations per chain
  total_iterations_per_chain <- stan_fit@sim$iter[1]  # Assuming all chains have the same number of iterations
  warmup_iterations_per_chain <- stan_fit@sim$warmup[1]  # Same assumption for warmup

  # Calculate the total number of iterations (total samples after warmup)
  total_iterations <- (total_iterations_per_chain - warmup_iterations_per_chain) * num_chains

  # Call the necessary diagnostic functions
  divergences <- rstan::get_num_divergent(stan_fit)
  divergences_percent <- (divergences / total_iterations) * 100

  treedepth <- rstan::get_num_max_treedepth(stan_fit)

  bfmi <- rstan::get_bfmi(stan_fit)

  # Format the output summary as a string
  diagnostics_summary <- paste(
    "HMC Diagnostics Summary:\n\n",
    "Divergences:\n",
    paste(divergences, "of", total_iterations, "iterations ended with a divergence (", round(divergences_percent, 2), "%).", "\nTry increasing 'adapt_delta' to remove the divergences.\n\n"),

    "Tree Depth:\n",
    paste(treedepth, "of", total_iterations, "iterations saturated the maximum tree depth.\nAdjust 'max_treedepth' to prevent saturation.\n\n"),

    "Energy:\n",
    "E-BFMI indicated possible pathological behavior:\n",
    paste("Chain", 1:length(bfmi), ": E-BFMI =", round(bfmi, 3), collapse = "\n"),
    "\nE-BFMI below 0.2 indicates you may need to reparameterize your model.\n",
    sep = ""
  )

  # Return the formatted summary as a character string
  return(diagnostics_summary)
}
