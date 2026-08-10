# =============================================================================
# Audit: fishery_catchgroups(observed_only = TRUE/FALSE) across fisheries
#
# Purpose
#   Independently reconstruct "which defined catch groups actually have observed
#   catch" from raw catch data, then diff that against what observed_only = TRUE
#   returns. Composite (piped) definitions are expanded to atomic combinations
#   before matching, so combined groups are evaluated correctly.
#
# Independence note (important)
#   This script derives atomics by string-splitting the composite label from
#   vw_model_catch_group. fishery_catchgroups() derives them from
#   vw_fishery_catch_group + model_catch_group (the atomic views).
#   These are deliberately different paths, so a disagreement is informative --
#   but it may indict the VIEW rather than the function. In particular, the previous
#   vw_model_catch_group bug (a piped Null dropped from the roll-up, e.g. a field
#   holding {Adult, Unknown, Null} rendering as "Adult|Unknown") would surface here
#   as a group where this script expects fewer atomics than the function sees.
#   Check any mismatch against vw_fishery_catch_group before filing it.
#
# Outputs (left in the global env)
#   audit_summary  - one row per fishery: status + counts + mismatch count
#   audit_detail   - one row per defined catch group: expected vs returned
#   audit_orphans  - observed atomic catch groups matching no defined group
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)

# ---- setup ------------------------------------------------------------------

devtools::load_all("C:/Repos/creelutils")   # <-- local creelutils checkout
print(packageVersion("creelutils"))         # confirm dev version (.9000+)

CG_COMPONENTS <- c("species", "life_stage", "fin_mark", "fate")

con <- connect_creel_db(db_env = "test")

cg_list <- fishery_catchgroups(conn = con)
# 1. Duplicate catch group check
cg_list |> count(fishery_name, combined_catch_group) |> filter(n > 1) |> arrange(desc(n))

# 2. Do piped SPECIES exist anywhere? (multi-species groups, untested edge case)
cg_list |> filter(grepl("|", species, fixed = TRUE)) |>
  distinct(fishery_name, combined_catch_group) # No they do not

# 3. Solo real-NA components (should no longer occur now that vw_model_catch_group fixed)
cg_list |> filter(if_any(all_of(CG_COMPONENTS), is.na)) |>
  distinct(fishery_name, combined_catch_group)

# 4. Most complex composites (_alt = alterations, use of OR operator)
cg_list |> mutate(n_alt = stringr::str_count(combined_catch_group, fixed("|"))) |>
  slice_max(n_alt, n = 15) |> select(fishery_name, combined_catch_group, n_alt)

# 5. Fisheries per definition-count, to find untested shapes
cg_list |> count(fishery_name, name = "n_defined") |> arrange(desc(n_defined)) |> print(n=100)

fishery_names <- c(
  "Skagit fall salmon 2023",           # max composite complexity (n_alt = 7), NA tokens, partial observation. baseline case
  "Skagit summer sockeye 2022",        # 7-alt composites x2; previous successful round-trip fishery
  "Cascade spring Chinook 2024",       # minimal (2 defs), NA as both piped token and solo literal "NA"
  "Humptulips winter steelhead 2025-26", # max definition count (29), all-atomic scale test
  "Chehalis salmon 2025",              # high count (25), all-atomic, partial observation, high volume
  "Snohomish fall salmon 2025",        # mid count (11), all-atomic, partial observation
  "Satsop winter steelhead 2025-26",   # duplicate label (1 of 28) on an UNOBSERVED group, masked by observed_only = TRUE
  "Hoh winter steelhead 2024-25",      # single definition, fully observed
  "Skagit summer sockeye 2026",        # zero catch records in 'test', shows cli_abort path
  "Skagit summer gamefish 2025",       # fishery exists, zero catch groups defined, shows  early-return path
)

# ---- helpers ----------------------------------------------------------------

# Expand pipe alternations in a definition table to one row per atomic combo.
expand_definitions <- function(cg) {
  cg |>
    dplyr::select(dplyr::all_of(c("combined_catch_group", CG_COMPONENTS))) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(CG_COMPONENTS),
      ~ tidyr::replace_na(as.character(.x), "NA")
    )) |>
    purrr::pmap(function(combined_catch_group, species, life_stage, fin_mark, fate) {
      tidyr::expand_grid(
        species    = strsplit(species,    "|", fixed = TRUE)[[1]],
        life_stage = strsplit(life_stage, "|", fixed = TRUE)[[1]],
        fin_mark   = strsplit(fin_mark,   "|", fixed = TRUE)[[1]],
        fate       = strsplit(fate,       "|", fixed = TRUE)[[1]]
      ) |>
        dplyr::mutate(
          combined_catch_group = combined_catch_group,
          atomic = combine_catch_group(dplyr::pick(dplyr::all_of(CG_COMPONENTS))),
          .before = 1
        )
    }) |>
    purrr::list_rbind()
}

# Observed atomic catch groups with record and fish counts.
summarise_observed <- function(catch) {
  if (NROW(catch) == 0L) {
    return(tibble::tibble(
      atomic = character(), n_records = integer(), n_fish = numeric()
    ))
  }
  catch |>
    dplyr::mutate(atomic = combine_catch_group(dplyr::pick(dplyr::all_of(CG_COMPONENTS)))) |>
    dplyr::group_by(.data$atomic) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      n_fish    = sum(.data$fish_count, na.rm = TRUE),
      .groups   = "drop"
    )
}

empty_summary <- function(fy, status, message = NA_character_) {
  tibble::tibble(
    fishery_name = fy, status = status,
    n_defined = NA_integer_, n_returned_true = NA_integer_,
    n_expected = NA_integer_, n_mismatch = NA_integer_,
    n_orphan_atomic = NA_integer_, n_orphan_fish = NA_real_,
    message = message
  )
}

# ---- per-fishery audit ------------------------------------------------------

audit_fishery <- function(fy, conn) {

  cat("\n----------------------------------------------------------\n")
  cat(fy, "\n")

  dat <- tryCatch(
    fetch_data(conn = conn, fishery_name = fy,
               tables = c("interview", "catch"), data_source = "internal"),
    error = function(e) e
  )
  if (inherits(dat, "error")) {
    cat("  ERROR in fetch_data():", conditionMessage(dat), "\n")
    return(list(summary = empty_summary(fy, "ERROR: fetch_data",
                                        conditionMessage(dat))))
  }

  cg_all <- tryCatch(
    fishery_catchgroups(conn = conn, fishery_name = fy, observed_only = FALSE),
    error = function(e) e
  )
  if (inherits(cg_all, "error")) {
    cat("  ERROR in fishery_catchgroups(FALSE):", conditionMessage(cg_all), "\n")
    return(list(summary = empty_summary(fy, "ERROR: observed_only = FALSE",
                                        conditionMessage(cg_all))))
  }
  if (nrow(cg_all) == 0L) {
    cat("  No catch groups defined for this fishery.\n")
    return(list(summary = empty_summary(fy, "NO DEFINITIONS")))
  }

  cg_obs <- tryCatch(
    fishery_catchgroups(conn = conn, fishery_name = fy, observed_only = TRUE),
    error = function(e) e
  )
  aborted <- inherits(cg_obs, "error")
  abort_class <- if (aborted) class(cg_obs)[1] else NA_character_

  # --- independent expectation ---
  defs   <- expand_definitions(cg_all)
  obs    <- summarise_observed(dat$catch)

  expected <- defs |>
    dplyr::distinct() |> # prevents duplicate cg from expanding n_fish
    dplyr::left_join(obs, by = "atomic") |>
    dplyr::group_by(.data$combined_catch_group) |>
    dplyr::summarise(
      n_atomic  = dplyr::n(),
      n_records = sum(.data$n_records, na.rm = TRUE),
      n_fish    = sum(.data$n_fish,    na.rm = TRUE),
      .groups   = "drop"
    ) |>
    dplyr::mutate(expected = .data$n_records > 0)

  returned_labels <- if (aborted) character() else cg_obs$combined_catch_group

  detail <- expected |>
    dplyr::mutate(
      fishery_name = fy,
      returned = if (aborted) NA else .data$combined_catch_group %in% returned_labels,
      flag = dplyr::case_when(
        aborted                        ~ "n/a (aborted)",
        expected  &  returned          ~ "ok: kept, observed",
        !expected & !returned          ~ "ok: dropped, unobserved",
        !expected &  returned          ~ "MISMATCH: kept but unobserved",
        expected  & !returned          ~ "MISMATCH: dropped but observed"
      )
    ) |>
    dplyr::select(
      "fishery_name", "combined_catch_group", "n_atomic",
      "n_records", "n_fish", "expected", "returned", "flag"
    ) |>
    dplyr::arrange(.data$combined_catch_group)

  # observed catch matching no defined group (expected/normal, but worth seeing)
  orphans <- obs |>
    dplyr::anti_join(dplyr::distinct(defs, .data$atomic), by = "atomic") |>
    dplyr::mutate(fishery_name = fy, .before = 1) |>
    dplyr::arrange(dplyr::desc(.data$n_fish))

  n_mismatch <- sum(grepl("^MISMATCH", detail$flag))

  status <- dplyr::case_when(
    aborted        ~ paste0("ABORT: ", abort_class),
    n_mismatch > 0 ~ "MISMATCH",
    TRUE           ~ "OK"
  )

  cat("  defined:", nrow(cg_all),
      "| expected observed:", sum(detail$expected),
      "| returned by TRUE:", if (aborted) "aborted" else length(returned_labels),
      "| mismatches:", n_mismatch, "\n")
  if (aborted) cat("  abort message:", conditionMessage(cg_obs), "\n")
  print(detail, n = 100)

  list(
    summary = tibble::tibble(
      fishery_name    = fy,
      status          = status,
      n_defined       = nrow(cg_all),
      n_returned_true = if (aborted) NA_integer_ else length(returned_labels),
      n_expected      = sum(detail$expected),
      n_mismatch      = n_mismatch,
      n_orphan_atomic = nrow(orphans),
      n_orphan_fish   = sum(orphans$n_fish),
      message         = if (aborted) conditionMessage(cg_obs) else NA_character_
    ),
    detail  = detail,
    orphans = orphans
  )
}

# ---- run --------------------------------------------------------------------

results <- purrr::map(fishery_names, audit_fishery, conn = con)

audit_summary <- purrr::map(results, "summary") |> purrr::list_rbind()
audit_summary$message <- cli::ansi_strip(audit_summary$message)
audit_detail  <- purrr::map(results, "detail")  |> purrr::compact() |> purrr::list_rbind()
audit_orphans <- purrr::map(results, "orphans") |> purrr::compact() |> purrr::list_rbind()

cat("\n\n================= SUMMARY =================\n")
print(audit_summary, n = 100, width = Inf)

if (any(audit_summary$status == "MISMATCH", na.rm = TRUE)) {
  cat("\n================= MISMATCH DETAIL =================\n")
  audit_detail |>
    dplyr::filter(grepl("^MISMATCH", .data$flag)) |>
    print(n = 100, width = Inf)
} else {
  cat("\nNo mismatches: observed_only agrees with independently derived expectation.\n")
}
