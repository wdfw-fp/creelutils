#' Summarize BSS model runtime
#'
#' @param stan_fit A 'stanfit' class object
#' @param output_file Directory to save output as text file
#' @export

get_bss_runtime <- function(stan_fit, output_file = NULL) {
  # Get sampling time
  raw_times <- rstan::get_elapsed_time(stan_fit)
  chain_times <- as.data.frame(raw_times)
  chain_times$chain <- paste0("Chain ", seq_len(nrow(chain_times)))
  chain_times$total <- rowSums(chain_times[, c("warmup", "sample")])

  # Convert to minutes and keep one decimal place, including trailing .0
  chain_times_min <- chain_times |>
    dplyr::mutate(
      dplyr::across(c(.data$warmup, .data$sample, .data$total), ~ format(round(.x / 60, 1), nsmall = 1))
    )

  total_elapsed <- format(round(max(chain_times$total) / 60, 1), nsmall = 1)

  # --- Console output ---
  cli::cli_h1("Stan model sampling summary")
  cli::cli_text("Runtime in minutes")

  for (i in seq_len(nrow(chain_times_min))) {
    row <- chain_times_min[i, ]
    cli::cli_li(
      "{.strong {row$chain}} — Warmup: {row$warmup} | Sample: {row$sample} | Total: {row$total}"
    )
  }

  cli::cli_alert_info("Total elapsed time (longest chain): {.val {total_elapsed}} minutes")

  # --- Write output file ---
  summary_str <- sprintf("Stan model sampling summary — %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  summary_str <- c(summary_str, "Runtime in minutes:")

  for (i in seq_len(nrow(chain_times_min))) {
    row <- chain_times_min[i, ]
    summary_str <- c(
      summary_str,
      sprintf(
        "• %s — Warmup: %s | Sample: %s | Total: %s",
        row$chain, row$warmup, row$sample, row$total
      )
    )
  }

  summary_str <- c(
    summary_str,
    sprintf("Total elapsed time (longest chain): %s minutes", total_elapsed)
  )

  if (!is.null(output_file)) {
    writeLines(summary_str, output_file)
  }

  invisible(list(
    timing_table = chain_times_min,
    total_elapsed = total_elapsed
  ))
}
