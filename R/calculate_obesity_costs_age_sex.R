#' Calculate obesity-attributable direct medical costs by age and sex
#'
#' Estimates obesity-attributable direct medical costs using outcome-level PAFs
#' calculated from age- and sex-specific simulated obesity prevalence and GBD
#' relative risks. Published overweight-and-obesity costs are rescaled using
#' the ratio of the obesity-only PAF to the published overweight-and-obesity
#' PAF.
#'
#' @param paf_by_outcome A data frame containing `outcome` and `paf_obesity`,
#'   such as the `paf_by_outcome` element returned by
#'   [calculate_obesity_pafs_age_sex()].
#' @param outcome_mapping A data frame mapping GBD outcome names to cost
#'   disease groups. It must contain `outcome_pattern` and `disease_group`.
#' @param costs_dro A data frame of direct medical costs with `disease_group`,
#'   `inpatient`, `outpatient`, `pharmacological`, and `total`.
#' @param paf_ow_ob A data frame of published overweight-and-obesity PAFs with
#'   `disease_group` and `paf_ow_ob`.
#' @param exclude_disease_groups Character vector of cost disease groups to
#'   exclude when no compatible relative-risk evidence is available.
#'
#' @return A list with `costs_by_group`, `paf_by_group`, and `total_costs`.
#' @export
calculate_obesity_costs_age_sex <- function(
    paf_by_outcome,
    outcome_mapping,
    costs_dro,
    paf_ow_ob,
    exclude_disease_groups = character()
) {
  required_paf <- c("outcome", "paf_obesity")
  required_mapping <- c("outcome_pattern", "disease_group")
  required_costs <- c(
    "disease_group",
    "inpatient",
    "outpatient",
    "pharmacological",
    "total"
  )
  required_paf_ow_ob <- c("disease_group", "paf_ow_ob")

  missing_paf <- setdiff(required_paf, names(paf_by_outcome))
  missing_mapping <- setdiff(required_mapping, names(outcome_mapping))
  missing_costs <- setdiff(required_costs, names(costs_dro))
  missing_paf_ow_ob <- setdiff(required_paf_ow_ob, names(paf_ow_ob))

  if (length(missing_paf) > 0L) {
    stop(
      "paf_by_outcome is missing required column(s): ",
      paste(missing_paf, collapse = ", "),
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

  if (length(missing_costs) > 0L) {
    stop(
      "costs_dro is missing required column(s): ",
      paste(missing_costs, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_paf_ow_ob) > 0L) {
    stop(
      "paf_ow_ob is missing required column(s): ",
      paste(missing_paf_ow_ob, collapse = ", "),
      call. = FALSE
    )
  }

  exclude_disease_groups <- as.character(exclude_disease_groups)

  known_disease_groups <- union(
    as.character(costs_dro$disease_group),
    as.character(paf_ow_ob$disease_group)
  )

  unknown_exclusions <- setdiff(
    exclude_disease_groups,
    known_disease_groups
  )

  if (length(unknown_exclusions) > 0L) {
    stop(
      "exclude_disease_groups contains unknown disease group(s): ",
      paste(unknown_exclusions, collapse = ", "),
      call. = FALSE
    )
  }

  paf_by_outcome <- paf_by_outcome |>
    dplyr::transmute(
      outcome_lc = tolower(as.character(outcome)),
      paf_obesity = as.numeric(paf_obesity)
    )

  if (any(!is.finite(paf_by_outcome$paf_obesity)) ||
      any(paf_by_outcome$paf_obesity < 0 |
          paf_by_outcome$paf_obesity > 1)) {
    stop(
      "paf_by_outcome contains invalid obesity PAF values.",
      call. = FALSE
    )
  }

  mapping <- outcome_mapping |>
    dplyr::transmute(
      outcome_lc = tolower(as.character(outcome_pattern)),
      disease_group
    )

  paf_by_group <- paf_by_outcome |>
    dplyr::left_join(
      mapping,
      by = "outcome_lc"
    ) |>
    dplyr::filter(!is.na(disease_group)) |>
    dplyr::group_by(disease_group) |>
    dplyr::summarise(
      paf_obesity = mean(paf_obesity, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      paf_ow_ob,
      by = "disease_group"
    )

  included_paf_ow_ob <- paf_ow_ob |>
    dplyr::filter(
      !disease_group %in% exclude_disease_groups
    )

  missing_groups <- included_paf_ow_ob |>
    dplyr::anti_join(
      paf_by_group,
      by = "disease_group"
    ) |>
    dplyr::pull(disease_group)

  if (length(missing_groups) > 0L) {
    stop(
      "No obesity PAF was available for non-excluded disease group(s): ",
      paste(missing_groups, collapse = ", "),
      call. = FALSE
    )
  }

  paf_by_group <- paf_by_group |>
    dplyr::filter(
      !disease_group %in% exclude_disease_groups
    )

  costs_by_group <- costs_dro |>
    dplyr::filter(
      !disease_group %in% exclude_disease_groups
    ) |>
    dplyr::left_join(
      paf_by_group,
      by = "disease_group"
    )

  if (any(is.na(costs_by_group$paf_obesity)) ||
      any(is.na(costs_by_group$paf_ow_ob))) {
    stop(
      "No obesity PAF was available for one or more non-excluded cost ",
      "disease groups.",
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
    paf_by_group = paf_by_group,
    total_costs = total_costs
  )
}
