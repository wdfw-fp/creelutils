#' Prep export
#'
#' Prepares standardized model estimates for database export. Joins the
#' catch group crosswalk (returned by `resolve_catch_groups()` in
#' CreelEstimates) to attach `model_catch_group_id` to catch and CPUE
#' estimate rows, joins angler type UUIDs on the stratum table, and drops
#' local-only name/label columns to match the model_estimates_stratum and
#' model_estimates_total schemas.
#'
#' Effort rows carry no catch group; their `model_catch_group_id` is NA and
#' written as NULL (the column is nullable on both tables).
#'
#' @family ETL
#' @param conn a valid `DBI` connection. @seealso [establish_db_con()]
#' @param creel_estimates list object containing standardized model
#'   estimates returned by `transform_estimates()`.
#' @param catch_group_lut catch group crosswalk for the fishery, returned by
#'   `resolve_catch_groups()`. Must contain `combined_catch_group` and
#'   `model_catch_group_id`.
#'
#' @return `creel_estimates` with `stratum` and `total` tables prepared for
#'   `write_stratum()` and `write_total()`.
#' @export
prep_export <- function(
    conn,
    creel_estimates,
    catch_group_lut
  ) {

  # Validate the crosswalk -------------------------------------------------
  required_cols <- c("combined_catch_group", "model_catch_group_id")
  missing_cols <- setdiff(required_cols, names(catch_group_lut))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "catch_group_lut is missing required column{?s}: {.field {missing_cols}}."
    )
  }

  # Duplicate labels would silently inflate estimate rows on join
  dup_labels <- catch_group_lut$combined_catch_group[
    duplicated(catch_group_lut$combined_catch_group)
  ]
  if (length(dup_labels) > 0) {
    cli::cli_abort(c(
      "Duplicate combined_catch_group label{?s} in catch_group_lut: {.val {unique(dup_labels)}}.",
      "x" = "Joining would inflate estimate rows."
    ))
  }

  cg_crosswalk <- catch_group_lut |> dplyr::select("combined_catch_group", "model_catch_group_id")

  # Fetch angler type UUIDs (stratum only) ---------------------------------
  angler_type_lut <- fetch_db_table(conn = conn, "creel", "angler_type_lut") |>
    dplyr::select("angler_type_code", "angler_type_id")

  # Helper: join crosswalk and validate resolution --------------------------
  # Invariant: every non-NA est_cg (catch and CPUE rows) must resolve to a
  # model_catch_group_id; effort rows have NA est_cg and remain NA.
  join_catch_groups <- function(df, table_name) {
    df <- df |>
      dplyr::left_join(
        cg_crosswalk,
        by = c("est_cg" = "combined_catch_group"),
        relationship = "many-to-one"
      )

    unresolved <- df |>
      dplyr::filter(!is.na(.data$est_cg), is.na(.data$model_catch_group_id)) |>
      dplyr::distinct(.data$est_cg)

    if (nrow(unresolved) > 0) {
      cli::cli_abort(c(
        "Unresolved catch group label{?s} in {.val {table_name}} estimates: {.val {unresolved$est_cg}}.",
        "x" = "No matching combined_catch_group in catch_group_lut.",
        "i" = "Estimate labels and the resolve_catch_groups() crosswalk are out of sync."
      ))
    }

    df
  }

  ##total --------------------------------------------------------------------
  creel_estimates$total <- creel_estimates$total |>
    join_catch_groups("total") |>
    dplyr::select(-"est_cg", -"project_name", -"fishery_name") |>
    dplyr::relocate("analysis_id", "model_catch_group_id")

  ## stratum ----------------------------------------------------------------
  creel_estimates$stratum <- creel_estimates$stratum |>
    join_catch_groups("stratum") |>
    dplyr::left_join(angler_type_lut,
                     by = c("angler_final" = "angler_type_code")) |>
    dplyr::select(-c("est_cg", "project_name", "fishery_name", "angler_final")) |>
    dplyr::relocate("analysis_id", "model_catch_group_id")

  #reformat NaN estimate values in stratum scale to 0 values
  creel_estimates$stratum <- creel_estimates$stratum |>
    dplyr::mutate(
      estimate_value = dplyr::case_when(
        is.nan(.data$estimate_value) ~ 0,
        TRUE ~ .data$estimate_value
    ))

  return(creel_estimates)
}
