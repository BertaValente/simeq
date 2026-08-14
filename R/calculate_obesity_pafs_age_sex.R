#' Calculate age- and sex-specific obesity population-attributable fractions
#'
#' Combines GBD relative risks with simulated obesity prevalence by sex and
#' five-year age band. Stratum-specific PAFs are aggregated to outcome-level
#' PAFs using the simulated represented population as weights. Protective
#' associations produce an obesity-attributable PAF of zero for cost estimation.
#' This function is intended for PONTE-PT simulations of adults aged 20 to
#' 84 years.
#'
#' @param rr_matrix A data frame returned by [read_gbd_bmi_rr()].
#' @param prevalence_age_sex A data frame returned by
#'   [extract_simulated_obesity_prevalence_by_age_sex()].
#' @param bmi_reference Reference BMI value.
#' @param bmi_obesity BMI value representing obesity.
#'
#' @return A list with `paf_by_stratum` and `paf_by_outcome`. The former
#'   contains PAFs by outcome, sex, and age band; the latter contains
#'   population-weighted PAFs by outcome.
#' @export
calculate_obesity_pafs_age_sex <- function(
    rr_matrix,
    prevalence_age_sex,
    bmi_reference = 22,
    bmi_obesity = 32
) {
  required_rr <- c("disease", "sex", "age_band", "rr_per_5")
  required_prevalence <- c(
    "year",
    "sex",
    "age_band",
    "population_n",
    "p_obesity"
  )

  missing_rr <- setdiff(required_rr, names(rr_matrix))
  missing_prevalence <- setdiff(
    required_prevalence,
    names(prevalence_age_sex)
  )

  if (length(missing_rr) > 0L) {
    stop(
      "rr_matrix is missing required column(s): ",
      paste(missing_rr, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_prevalence) > 0L) {
    stop(
      "prevalence_age_sex is missing required column(s): ",
      paste(missing_prevalence, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(bmi_reference) != 1L ||
      length(bmi_obesity) != 1L ||
      !is.finite(bmi_reference) ||
      !is.finite(bmi_obesity) ||
      bmi_obesity <= bmi_reference) {
    stop(
      "bmi_obesity must be one finite value greater than bmi_reference.",
      call. = FALSE
    )
  }

  prevalence_age_sex <- prevalence_age_sex |>
    dplyr::mutate(
      sex = as.character(sex),
      age_band = as.character(age_band),
      population_n = as.numeric(population_n),
      p_obesity = as.numeric(p_obesity)
    )

  if (anyDuplicated(
    prevalence_age_sex[c("year", "sex", "age_band")]
  ) > 0L) {
    stop(
      "prevalence_age_sex must contain at most one row per year, sex, ",
      "and age band.",
      call. = FALSE
    )
  }

  if (any(!is.finite(prevalence_age_sex$population_n)) ||
      any(prevalence_age_sex$population_n <= 0) ||
      any(!is.finite(prevalence_age_sex$p_obesity)) ||
      any(prevalence_age_sex$p_obesity < 0 |
          prevalence_age_sex$p_obesity > 1)) {
    stop(
      "prevalence_age_sex contains invalid population weights or obesity ",
      "prevalence values.",
      call. = FALSE
    )
  }

  scale_factor <- (bmi_obesity - bmi_reference) / 5

  rr_obesity <- rr_matrix |>
    dplyr::transmute(
      outcome = as.character(disease),
      sex = as.character(sex),
      age_band = as.character(age_band),
      rr_obesity_vs_normal = as.numeric(rr_per_5) ^ scale_factor
    )

  if (any(!is.finite(rr_obesity$rr_obesity_vs_normal)) ||
      any(rr_obesity$rr_obesity_vs_normal <= 0)) {
    stop(
      "rr_matrix contains invalid relative-risk estimates.",
      call. = FALSE
    )
  }

  paf_by_stratum <- rr_obesity |>
    dplyr::inner_join(
      prevalence_age_sex,
      by = c("sex", "age_band")
    ) |>
    dplyr::mutate(
      paf_obesity_raw = (
        p_obesity * (rr_obesity_vs_normal - 1)
      ) / (
        1 + p_obesity * (rr_obesity_vs_normal - 1)
      ),
      paf_obesity = pmax(paf_obesity_raw, 0)
    )

  if (nrow(paf_by_stratum) == 0L) {
    stop(
      "No GBD relative-risk rows matched the simulated age-sex strata.",
      call. = FALSE
    )
  }

  if (any(!is.finite(paf_by_stratum$paf_obesity_raw)) ||
      any(!is.finite(paf_by_stratum$paf_obesity)) ||
      any(paf_by_stratum$paf_obesity < 0 |
          paf_by_stratum$paf_obesity > 1)) {
    stop(
      "Invalid PAF values were produced.",
      call. = FALSE
    )
  }

  paf_by_outcome <- paf_by_stratum |>
    dplyr::group_by(outcome) |>
    dplyr::summarise(
      paf_obesity = stats::weighted.mean(
        paf_obesity,
        w = population_n,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(outcome)

  list(
    paf_by_stratum = paf_by_stratum,
    paf_by_outcome = paf_by_outcome
  )
}
