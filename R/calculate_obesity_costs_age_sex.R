#' Apply high-BMI PAFs to published disease costs
#'
#' Rescales published direct medical costs using simulation-derived
#' population-attributable fractions (PAFs) for high BMI. The PAFs are
#' applied directly to published disease costs — not via a ratio to a
#' separately published PAF. This function accepts the `paf_by_outcome`
#' output from [calculate_obesity_pafs_age_sex()].
#'
#' @param paf_by_outcome A data frame with `outcome` and `paf_obesity`
#'   (population-weighted PAF per GBD outcome), as returned by
#'   [calculate_obesity_pafs_age_sex()].
#' @param outcome_mapping A data frame mapping GBD outcome names to cost
#'   disease groups. Must contain `outcome_pattern` and `disease_group`.
#' @param costs_dro A data frame of published direct medical costs. Must
#'   contain `disease_group`, `inpatient`, `outpatient`, `pharmacological`,
#'   and `total`.
#' @param exclude_disease_groups Character vector of disease groups to exclude
#'   from attributable-cost estimation (e.g. those without GBD RR evidence).
#'
#' @return A list with `costs_by_group`, `paf_by_group`, and `total_costs`.
#' @export
calculate_obesity_costs_age_sex <- function(
    paf_by_outcome,
    outcome_mapping,
    costs_dro,
    exclude_disease_groups = character()
) {
  required_paf     <- c("outcome", "paf_obesity")
  required_mapping <- c("outcome_pattern", "disease_group")
  required_costs   <- c("disease_group", "inpatient", "outpatient",
                        "pharmacological", "total")

  for (nm in list(
    list(required_paf,     paf_by_outcome,  "paf_by_outcome"),
    list(required_mapping, outcome_mapping, "outcome_mapping"),
    list(required_costs,   costs_dro,       "costs_dro")
  )) {
    missing <- setdiff(nm[[1L]], names(nm[[2L]]))
    if (length(missing) > 0L) {
      stop(nm[[3L]], " is missing required column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
  }

  exclude_disease_groups <- as.character(exclude_disease_groups)

  unknown_exclusions <- setdiff(
    exclude_disease_groups, as.character(costs_dro$disease_group)
  )
  if (length(unknown_exclusions) > 0L) {
    stop(
      "exclude_disease_groups contains unknown disease group(s): ",
      paste(unknown_exclusions, collapse = ", "), call. = FALSE
    )
  }

  # Map GBD outcome names to cost disease groups and average across outcomes
  # that map to the same disease group
  mapping <- outcome_mapping |>
    dplyr::transmute(
      outcome_lc    = tolower(as.character(outcome_pattern)),
      disease_group
    )

  paf_by_group <- paf_by_outcome |>
    dplyr::transmute(
      outcome_lc  = tolower(as.character(outcome)),
      paf_obesity = as.numeric(paf_obesity)
    ) |>
    dplyr::left_join(mapping, by = "outcome_lc") |>
    dplyr::filter(!is.na(disease_group)) |>
    dplyr::group_by(disease_group) |>
    dplyr::summarise(
      paf_obesity = mean(paf_obesity, na.rm = TRUE),
      .groups     = "drop"
    )

  if (any(!is.finite(paf_by_group$paf_obesity)) ||
      any(paf_by_group$paf_obesity < 0 | paf_by_group$paf_obesity > 1)) {
    stop("paf_by_group contains invalid PAF values.", call. = FALSE)
  }

  # Join PAFs to costs and apply directly
  costs_by_group <- costs_dro |>
    dplyr::filter(!disease_group %in% exclude_disease_groups) |>
    dplyr::left_join(paf_by_group, by = "disease_group")

  if (any(is.na(costs_by_group$paf_obesity))) {
    missing_groups <- costs_by_group |>
      dplyr::filter(is.na(paf_obesity)) |>
      dplyr::pull(disease_group)
    stop(
      "No PAF was available for non-excluded cost disease group(s): ",
      paste(missing_groups, collapse = ", "), call. = FALSE
    )
  }

  costs_by_group <- costs_by_group |>
    dplyr::mutate(
      inpatient_obesity        = inpatient        * paf_obesity,
      outpatient_obesity       = outpatient       * paf_obesity,
      pharmacological_obesity  = pharmacological  * paf_obesity,
      total_obesity            = total            * paf_obesity
    )

  total_costs <- costs_by_group |>
    dplyr::summarise(
      inpatient_obesity_total       = sum(inpatient_obesity,       na.rm = TRUE),
      outpatient_obesity_total      = sum(outpatient_obesity,      na.rm = TRUE),
      pharmacological_obesity_total = sum(pharmacological_obesity, na.rm = TRUE),
      total_obesity                 = sum(total_obesity,           na.rm = TRUE)
    )

  list(
    costs_by_group = costs_by_group,
    paf_by_group   = paf_by_group,
    total_costs    = total_costs
  )
}
