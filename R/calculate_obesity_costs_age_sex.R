#' Apply high-BMI PAFs to published disease costs (ratio-rescaling)
#'
#' Rescales published direct medical costs using a ratio of simulation-derived
#' PAFs to published (Borges et al.) reference PAFs.
#'
#' The published cost matrix from Borges et al. (2025) contains costs that are
#' **already obesity-attributable** — i.e. total disease costs multiplied by
#' the published PAFs. Applying a second PAF directly would double-penalise
#' the estimate. Instead, this function computes the adjustment factor as:
#'
#' \deqn{\text{adjustment} = \frac{PAF_{\text{simulated}}}{PAF_{\text{Borges}}}}
#'
#' which is equivalent to back-calculating the total disease cost and then
#' re-applying the simulated PAF.
#'
#' @param paf_by_outcome A data frame with `outcome` and `paf_obesity`
#'   (population-weighted PAF per GBD outcome), as returned by
#'   [calculate_obesity_pafs_individual()].
#' @param outcome_mapping A data frame mapping GBD outcome names to cost
#'   disease groups. Must contain `outcome_pattern` and `disease_group`.
#' @param costs_dro A data frame of **published obesity-attributable** direct
#'   medical costs (post-PAF, as reported in Borges et al. 2025). Must contain
#'   `disease_group`, `inpatient`, `outpatient`, `pharmacological`, and
#'   `total`.
#' @param reference_paf_by_group A data frame with `disease_group` and
#'   `paf_reference` columns giving the published Borges PAFs by disease group.
#'   Obtain from [obesity_overweight_pafs()] and rename `paf_ow_ob` to
#'   `paf_reference` before passing. All disease groups in `costs_dro` (that
#'   are not excluded) must have a matching reference PAF.
#' @param exclude_disease_groups Character vector of disease groups to exclude
#'   from attributable-cost estimation (e.g. those without GBD RR evidence).
#'
#' @return A list with:
#' \describe{
#'   \item{costs_by_group}{Per-disease-group adjusted attributable costs,
#'     including `paf_obesity` (simulated), `paf_reference` (Borges), and
#'     `paf_ratio`.}
#'   \item{paf_by_group}{Simulation-derived PAFs collapsed to disease-group
#'     level.}
#'   \item{total_costs}{Total attributable costs summed across disease groups.}
#' }
#' @export
calculate_obesity_costs_age_sex <- function(
    paf_by_outcome,
    outcome_mapping,
    costs_dro,
    reference_paf_by_group = NULL,
    exclude_disease_groups = character()
) {
  required_paf     <- c("outcome", "paf_obesity")
  required_mapping <- c("outcome_pattern", "disease_group")
  required_costs   <- c("disease_group", "inpatient", "outpatient",
                        "pharmacological", "total")
  required_ref     <- c("disease_group", "paf_reference")

  validation_list <- list(
    list(required_paf,     paf_by_outcome,  "paf_by_outcome"),
    list(required_mapping, outcome_mapping, "outcome_mapping"),
    list(required_costs,   costs_dro,       "costs_dro")
  )
  if (!is.null(reference_paf_by_group)) {
    validation_list <- c(
      validation_list,
      list(list(required_ref, reference_paf_by_group, "reference_paf_by_group"))
    )
  }

  for (nm in validation_list) {
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

  # ── Map GBD outcomes to cost disease groups ─────────────────────────────────
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

  # ── Validate and prepare reference PAFs ─────────────────────────────────────
  # When reference_paf_by_group is NULL, PAFs are applied directly (no
  # ratio rescaling). This preserves backward compatibility.
  if (!is.null(reference_paf_by_group)) {
    ref_paf <- reference_paf_by_group |>
      dplyr::transmute(
        disease_group,
        paf_reference = as.numeric(paf_reference)
      )
    if (any(!is.finite(ref_paf$paf_reference)) ||
        any(ref_paf$paf_reference <= 0 | ref_paf$paf_reference > 1)) {
      stop("`reference_paf_by_group` contains invalid PAF values (must be in (0, 1]).",
           call. = FALSE)
    }
  } else {
    ref_paf <- NULL
  }

  # ── Join PAFs and costs; compute ratio-rescaled attributable costs ──────────
  costs_by_group <- costs_dro |>
    dplyr::filter(!disease_group %in% exclude_disease_groups) |>
    dplyr::left_join(paf_by_group, by = "disease_group")

  if (!is.null(ref_paf)) {
    costs_by_group <- costs_by_group |>
      dplyr::left_join(ref_paf, by = "disease_group")
  } else {
    # No reference PAF supplied: apply simulated PAF directly to costs
    # (costs_dro contains total disease costs, not attributable costs).
    costs_by_group$paf_reference <- 1.0
  }

  if (any(is.na(costs_by_group$paf_obesity))) {
    missing_groups <- costs_by_group |>
      dplyr::filter(is.na(paf_obesity)) |>
      dplyr::pull(disease_group)
    stop(
      "No simulated PAF available for non-excluded disease group(s): ",
      paste(missing_groups, collapse = ", "), call. = FALSE
    )
  }

  if (any(is.na(costs_by_group$paf_reference))) {
    missing_ref <- costs_by_group |>
      dplyr::filter(is.na(paf_reference)) |>
      dplyr::pull(disease_group)
    stop(
      "No reference PAF available for non-excluded disease group(s): ",
      paste(missing_ref, collapse = ", "), call. = FALSE
    )
  }

  # Ratio: simulated PAF / Borges PAF.
  # Multiplying the published attributable cost by this ratio is equivalent to
  # back-calculating the total disease cost (÷ paf_reference) and then applying
  # the simulated PAF (× paf_obesity).
  costs_by_group <- costs_by_group |>
    dplyr::mutate(
      paf_ratio               = paf_obesity / paf_reference,
      inpatient_obesity       = inpatient       * paf_ratio,
      outpatient_obesity      = outpatient      * paf_ratio,
      pharmacological_obesity = pharmacological * paf_ratio,
      total_obesity           = total           * paf_ratio
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
