#' Calculate obesity-attributable direct medical costs
#'
#' Estimates obesity-attributable direct medical costs using a
#' cost-of-illness approach. It rescales published overweight-and-obesity
#' disease costs using obesity-only population-attributable fractions (PAFs).
#'
#' @param rr_obesity A data frame containing `outcome`, `sex`, and
#'   `rr_obesity_vs_normal`.
#' @param outcome_mapping A data frame mapping GBD outcome names to
#'   cost `disease_group` values. It must contain `outcome_pattern` and
#'   `disease_group`.
#' @param prevalence A data frame containing sex-specific obesity prevalence.
#'   It must contain `sex` and `p_obesity`, where prevalence is a proportion
#'   between 0 and 1.
#' @param costs_dro A data frame of direct medical costs. It must contain
#'   `disease_group`, `inpatient`, `outpatient`, `pharmacological`, and `total`.
#' @param paf_ow_ob A data frame of overweight-and-obesity PAFs by disease
#'   group. It must contain `disease_group` and `paf_ow_ob`.
#'
#' @return A list with disease-group costs, PAFs, and a total-cost summary.
#' @export
calculate_obesity_costs <- function(
    rr_obesity,
    outcome_mapping,
    prevalence,
    costs_dro,
    paf_ow_ob
) {
  required_rr <- c("outcome", "sex", "rr_obesity_vs_normal")
  required_mapping <- c("outcome_pattern", "disease_group")
  required_prevalence <- c("sex", "p_obesity")
  required_costs <- c(
    "disease_group",
    "inpatient",
    "outpatient",
    "pharmacological",
    "total"
  )
  required_paf <- c("disease_group", "paf_ow_ob")

  missing_rr <- setdiff(required_rr, names(rr_obesity))
  missing_mapping <- setdiff(required_mapping, names(outcome_mapping))
  missing_prevalence <- setdiff(required_prevalence, names(prevalence))
  missing_costs <- setdiff(required_costs, names(costs_dro))
  missing_paf <- setdiff(required_paf, names(paf_ow_ob))

  if (length(missing_rr) > 0L) {
    stop(
      "rr_obesity is missing required column(s): ",
      paste(missing_rr, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_mapping) > 0L) {
    stop(
      "outcome_mapping is missing required column(s): ",
      paste(missing_mapping, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_prevalence) > 0L) {
    stop(
      "prevalence is missing required column(s): ",
      paste(missing_prevalence, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_costs) > 0L) {
    stop(
      "costs_dro is missing required column(s): ",
      paste(missing_costs, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_paf) > 0L) {
    stop(
      "paf_ow_ob is missing required column(s): ",
      paste(missing_paf, collapse = ", "),
      call. = FALSE
    )
  }

  prevalence <- prevalence |>
    dplyr::mutate(
      sex = as.character(sex),
      p_obesity = as.numeric(p_obesity)
    )

  if (anyDuplicated(prevalence$sex) > 0L) {
    stop(
      "prevalence must contain no more than one row per sex.",
      call. = FALSE
    )
  }

  if (any(!is.finite(prevalence$p_obesity)) ||
      any(prevalence$p_obesity < 0 | prevalence$p_obesity > 1)) {
    stop(
      "p_obesity must contain finite proportions between 0 and 1.",
      call. = FALSE
    )
  }

  if (!all(c("Males", "Females") %in% prevalence$sex)) {
    stop(
      "prevalence must include both 'Males' and 'Females'.",
      call. = FALSE
    )
  }

  outcome_mapping_lc <- outcome_mapping |>
    dplyr::transmute(
      outcome_lc = tolower(outcome_pattern),
      disease_group
    )

  paf_obesity <- rr_obesity |>
    dplyr::mutate(
      outcome_lc = tolower(outcome)
    ) |>
    dplyr::left_join(
      prevalence,
      by = "sex"
    ) |>
    dplyr::mutate(
      paf_obesity = (
        p_obesity * (rr_obesity_vs_normal - 1)
      ) / (
        p_obesity * (rr_obesity_vs_normal - 1) + 1
      )
    ) |>
    dplyr::left_join(
      outcome_mapping_lc,
      by = "outcome_lc"
    ) |>
    dplyr::filter(!is.na(disease_group)) |>
    dplyr::group_by(disease_group) |>
    dplyr::summarise(
      paf_obesity = mean(paf_obesity, na.rm = TRUE),
      .groups = "drop"
    )

  paf_joined <- paf_ow_ob |>
    dplyr::left_join(
      paf_obesity,
      by = "disease_group"
    )

  missing_paf_groups <- paf_joined |>
    dplyr::filter(
      !is.finite(paf_ow_ob) |
        paf_ow_ob <= 0 |
        !is.finite(paf_obesity)
    ) |>
    dplyr::pull(disease_group)

  if (length(missing_paf_groups) > 0L) {
    stop(
      "Missing or invalid PAF values for disease group(s): ",
      paste(missing_paf_groups, collapse = ", "),
      call. = FALSE
    )
  }

  costs_by_group <- costs_dro |>
    dplyr::left_join(
      paf_joined,
      by = "disease_group"
    )

  missing_cost_groups <- costs_by_group |>
    dplyr::filter(
      is.na(paf_ow_ob) |
        is.na(paf_obesity)
    ) |>
    dplyr::pull(disease_group)

  if (length(missing_cost_groups) > 0L) {
    stop(
      "No PAF was available for cost disease group(s): ",
      paste(missing_cost_groups, collapse = ", "),
      call. = FALSE
    )
  }

  costs_by_group <- costs_by_group |>
    dplyr::mutate(
      scale_factor_paf = paf_obesity / paf_ow_ob,
      inpatient_obesity = inpatient * scale_factor_paf,
      outpatient_obesity = outpatient * scale_factor_paf,
      pharmacological_obesity = pharmacological * scale_factor_paf,
      total_obesity = total * scale_factor_paf
    )

  if (any(!is.finite(costs_by_group$total_obesity))) {
    stop(
      "Non-finite obesity-attributable cost estimates were produced.",
      call. = FALSE
    )
  }

  total_costs <- costs_by_group |>
    dplyr::summarise(
      inpatient_obesity_total = sum(inpatient_obesity, na.rm = TRUE),
      outpatient_obesity_total = sum(outpatient_obesity, na.rm = TRUE),
      pharmacological_obesity_total = sum(
        pharmacological_obesity,
        na.rm = TRUE
      ),
      total_obesity = sum(total_obesity, na.rm = TRUE)
    )

  list(
    costs_by_group = costs_by_group,
    paf_by_group = paf_joined,
    total_costs = total_costs
  )
}
