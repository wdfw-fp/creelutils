# Summarize HMC Diagnostics from a Stan Fit

Computes a human-readable summary of key Hamiltonian Monte Carlo (HMC)
diagnostics from a fitted Stan model object, including divergences, tree
depth saturation, and E-BFMI values across chains.

## Usage

``` r
get_hmc_diagnostics(stan_fit)
```

## Arguments

- stan_fit:

  A fitted Stan model object of class `stanfit`, typically created using
  [`rstan::stan()`](https://mc-stan.org/rstan/reference/stan.html) or
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Value

A single character string summarizing the following diagnostics:

- Number and percentage of post-warmup iterations with divergences.

- Number of iterations that saturated the maximum tree depth.

- E-BFMI values for each chain, indicating potential energy sampling
  issues.

## Details

This function reconstructs diagnostics similar to
[`rstan::check_hmc_diagnostics()`](https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html)
but returns a formatted character string instead of printing to the
console. This allows the output to be saved, displayed, or logged
programmatically.

This function summarizes key Hamiltonian Monte Carlo (HMC) convergence
diagnostics from a Stan model fit. It is intended to help users assess
whether further tuning is needed using available options like
`adapt_delta`, `max_treedepth`, or prior adjustments.

The diagnostics reported include:

- **Divergent transitions**: Should be less than 5% of post-warmup
  iterations. Divergences may indicate poor exploration of the posterior
  and often require increasing `adapt_delta` (e.g., from 0.75 up toward
  0.95 or 0.99) to reduce the step size. Smaller steps improve numerical
  stability but increase runtime.

- **Maximum tree depth**: No iterations should saturate the maximum tree
  depth. Saturation may co-occur with high `adapt_delta` due to long
  trajectories. If frequent, consider increasing `max_treedepth` above
  the default (usually 10–12).

- **E-BFMI**: This metric checks whether the energy transitions in HMC
  are efficient. E-BFMI below 0.2 is a sign of potential sampling
  issues, though reparameterization (typically recommended) is not
  accessible in this workflow. Instead, consider revisiting prior
  distributions to improve geometry.

This function is adapted to return results as text, in contrast to
[`rstan::check_hmc_diagnostics()`](https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html)
which prints directly to the console.
