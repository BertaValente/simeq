#' Calculate age- and sex-specific high-BMI population-attributable fractions
#'
#' Combines GBD relative risks with simulated overweight and obesity prevalence
#' by sex and five-year age band, using the correct multi-category PAF formula.
#' When `bmi_overweight` is supplied, both overweight (BMI 25.0–29.9 kg/m²)
#' and obesity (BMI ≥ 30 kg/m²) contribute to the combined high-BMI PAF:
#'
#' \deqn{
#'   PAF = \frac{
#'     p_{OW}(RR_{OW}-1) + p_{OB}(RR_{OB}-1)
#'   }{
#'     1 + p_{OW}(RR_{OW}-1) + p_{OB}(RR_{OB}-1)
#'   }
#' }
#'
#' where normal weight and underweight are treated as the reference (RR = 1).
#' Protective associations (RR < 1) produce a PAF clamped to zero for cost
#' estimation. Stratum-specific PAFs are aggregated to outcome-level PAFs using
#' the simulated represented population as weights.
#'
#' @param rr_matrix A data frame returned by [read_gbd_bmi_rr()].
#' @param prevalence_age_sex A data frame returned by
#'   [extract_simulated_obesity_prevalence_by_age_sex()], containing `p_obesity`
#'   and, when `bmi_overweight` is provided, `p_overweight`.
#' @param bmi_reference Reference (normal-weight) BMI value. Default 22 kg/m².
#' @param bmi_obesity Representative obesity BMI value. Default 32 kg/m².
#' @param bmi_overweight Optional representative overweight BMI value (e.g.
#'   27.5 kg/m²). When supplied, overweight is included in the PAF using the
#'   two-category multi-exposure formula.
#'
#' @return A list with `paf_by_stratum` (PAFs by outcome, sex, and age band)
#'   and `paf_by_outcome` (population-weighted PAFs by outcome).
#' @export
calculate_obesity_pafs_age_sex <- function(
    rr_matrix,
    prevalence_age_sex,
    bmi_reference  = 22,
    bmi_obesity    = 32,
    bmi_overweight = NULL
) {
  required_rr <- c("disease", "sex", "age_band", "rr_per_5")
  required_prevalence <- c("year", "sex", "age_band", "population_n", "p_obesity")

  missing_rr <- setdiff(required_rr, names(rr_matrix))
  missing_prevalence <- setdiff(required_prevalence, names(prevalence_age_sex))

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

  if (!is.null(bmi_overweight)) {
    if (!"p_overweight" %in% names(prevalence_age_sex)) {
      stop(
        "prevalence_age_sex must contain 'p_overweight' when bmi_overweight ",
        "is supplied.",
        call. = FALSE
      )
    }
    if (length(bmi_overweight) != 1L || !is.finite(bmi_overweight) ||
        bmi_overweight <= bmi_reference || bmi_overweight >= bmi_obesity) {
      stop(
        "bmi_overweight must be one finite value between bmi_reference and ",
        "bmi_obesity.",
        call. = FALSE
      )
    }
  }

  if (length(bmi_reference) != 1L || length(bmi_obesity) != 1L ||
      !is.finite(bmi_reference) || !is.finite(bmi_obesity) ||
      bmi_obesity <= bmi_reference) {
    stop(
      "bmi_obesity must be one finite value greater than bmi_reference.",
      call. = FALSE
    )
  }

  prevalence_age_sex <- prevalence_age_sex |>
    dplyr::mutate(
      sex          = as.character(sex),
      age_band     = as.character(age_band),
      population_n = as.numeric(population_n),
      p_obesity    = as.numeric(p_obesity)
    )

  if (anyDuplicated(prevalence_age_sex[c("year", "sex", "age_band")]) > 0L) {
    stop(
      "prevalence_age_sex must contain at most one row per year, sex, and ",
      "age band.",
      call. = FALSE
    )
  }

  scale_ob <- (bmi_obesity - bmi_reference) / 5

  # Build outcome × sex × age_band RR table
  rr_joined <- rr_matrix |>
    dplyr::transmute(
      outcome   = as.character(disease),
      sex       = as.character(sex),
      age_band  = as.character(age_band),
      rr_ob     = as.numeric(rr_per_5) ^ scale_ob
    )

  if (!is.null(bmi_overweight)) {
    scale_ow <- (bmi_overweight - bmi_reference) / 5
    rr_joined <- rr_joined |>
      dplyr::mutate(rr_ow = as.numeric(rr_matrix$rr_per_5) ^ scale_ow)
  }

  if (any(!is.finite(rr_joined$rr_ob)) || any(rr_joined$rr_ob <= 0)) {
    stop("rr_matrix contains invalid relative-risk estimates.", call. = FALSE)
  }

  paf_by_stratum <- rr_joined |>
    dplyr::inner_join(prevalence_age_sex, by = c("sex", "age_band"))

  if (nrow(paf_by_stratum) == 0L) {
    stop(
      "No GBD relative-risk rows matched the simulated age-sex strata.",
      call. = FALSE
    )
  }

  if (!is.null(bmi_overweight)) {
    # Two-category multi-exposure PAF formula
    paf_by_stratum <- paf_by_stratum |>
      dplyr::mutate(
        p_overweight = as.numeric(p_overweight),
        num = p_overweight * (rr_ow - 1) + p_obesity * (rr_ob - 1),
        den = 1 + p_overweight * (rr_ow - 1) + p_obesity * (rr_ob - 1),
        paf_high_bmi_raw = num / den,
        paf_obesity      = pmax(paf_high_bmi_raw, 0)  # clamp protective to 0
      ) |>
      dplyr::select(-num, -den, -paf_high_bmi_raw)
  } else {
    # Single-category (obesity-only) formula
    paf_by_stratum <- paf_by_stratum |>
      dplyr::mutate(
        paf_obesity_raw = p_obesity * (rr_ob - 1) / (1 + p_obesity * (rr_ob - 1)),
        paf_obesity     = pmax(paf_obesity_raw, 0)
      ) |>
      dplyr::select(-paf_obesity_raw)
  }

  if (any(!is.finite(paf_by_stratum$paf_obesity)) ||
      any(paf_by_stratum$paf_obesity < 0 | paf_by_stratum$paf_obesity > 1)) {
    stop("Invalid PAF values were produced.", call. = FALSE)
  }

  paf_by_outcome <- paf_by_stratum |>
    dplyr::group_by(outcome) |>
    dplyr::summarise(
      paf_obesity = stats::weighted.mean(
        paf_obesity, w = population_n, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(outcome)

  list(
    paf_by_stratum = paf_by_stratum,
    paf_by_outcome = paf_by_outcome
  )
}
