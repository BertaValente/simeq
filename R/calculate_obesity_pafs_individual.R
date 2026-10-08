#' Disease PAFs from individual simulated BMI
#'
#' Computes disease-specific population-attributable fractions from each
#' simulated person's BMI rather than from representative category BMIs.
#' For every person with BMI at or above `exposure_min_bmi`, the relative risk
#' is `rr_per_5 ^ ((min(bmi, bmi_cap) - bmi_reference) / 5)`; everyone else
#' has a relative risk of 1. Within each sex and 5-year age band,
#' \deqn{PAF = (\bar{RR} - 1) / \bar{RR}}
#' where \eqn{\bar{RR}} is the survey-weighted mean relative risk. Stratum
#' PAFs are averaged to national PAFs with population weights.
#'
#' @param simulation_output A data frame from [simeq_run_simulation()].
#' @param year Simulation year.
#' @param rr_matrix A data frame returned by [read_gbd_bmi_rr()].
#' @param bmi_reference Reference BMI (kg/m²). Default 22.
#' @param exposure_min_bmi Lowest BMI counted as exposed: 25 for high BMI
#'   (overweight and obesity, default) or 30 for obesity only.
#' @param bmi_cap BMI above which relative risks are not extrapolated.
#'   Default 50.
#' @param weight_col Survey-weight column. Default `"survey_weight"`.
#'
#' @return A list with `paf_by_stratum` and `paf_by_outcome` (columns
#'   `outcome`, `paf_obesity`).
#' @export
calculate_obesity_pafs_individual <- function(
    simulation_output,
    year,
    rr_matrix,
    bmi_reference = 22,
    exposure_min_bmi = 25,
    bmi_cap = 50,
    weight_col = "survey_weight"
) {
  required_rr <- c("disease", "sex", "age_band", "rr_per_5")
  missing_rr <- setdiff(required_rr, names(rr_matrix))
  if (length(missing_rr) > 0L) {
    stop(
      "rr_matrix is missing required column(s): ",
      paste(missing_rr, collapse = ", "),
      call. = FALSE
    )
  }

  year <- as.integer(year)
  people <- simulation_output[
    simulation_output$year == year &
      simulation_output$alive &
      !is.na(simulation_output$age) &
      simulation_output$age >= 20L &
      simulation_output$age <= 84L &
      !is.na(simulation_output$bmi) &
      !is.na(simulation_output[[weight_col]]) &
      simulation_output[[weight_col]] > 0,
    ,
    drop = FALSE
  ]
  if (nrow(people) == 0L) {
    stop("No living simulated people in year ", year, ".", call. = FALSE)
  }

  band_start <- ((as.integer(people$age) - 20L) %/% 5L) * 5L + 20L
  people <- data.frame(
    sex = dplyr::recode(
      as.character(people$sex),
      men = "Males", women = "Females"
    ),
    age_band = paste0(band_start, "-", band_start + 4L),
    weight = people[[weight_col]],
    excess_units = ifelse(
      people$bmi >= exposure_min_bmi,
      pmax(pmin(people$bmi, bmi_cap) - bmi_reference, 0) / 5,
      0
    ),
    stringsAsFactors = FALSE
  )

  rr <- rr_matrix |>
    dplyr::transmute(
      outcome  = as.character(disease),
      sex      = dplyr::recode(
        as.character(sex),
        men   = "Males",  Male   = "Males",  male   = "Males",
        women = "Females", Female = "Females", female = "Females"
      ),
      age_band = as.character(age_band),
      rr_per_5 = as.numeric(rr_per_5)
    )

  if (any(!is.finite(rr$rr_per_5)) || any(rr$rr_per_5 <= 0)) {
    stop("rr_matrix contains invalid relative-risk estimates.", call. = FALSE)
  }

  # Mean RR within each sex x age band, vectorised over outcomes: only exposed
  # people (excess_units > 0) need rr^units; everyone else has RR = 1. This
  # avoids a people x outcomes join, which is slow for large simulations.
  stratum_key <- paste(people$sex, people$age_band, sep = "|")
  people_by_stratum <- split(people, stratum_key)
  rr_by_stratum <- split(rr, paste(rr$sex, rr$age_band, sep = "|"))

  matched_strata <- intersect(names(people_by_stratum), names(rr_by_stratum))
  if (length(matched_strata) == 0L) {
    stop(
      "No GBD relative-risk rows matched the simulated age-sex strata.",
      call. = FALSE
    )
  }

  paf_by_stratum <- lapply(
    matched_strata,
    function(key) {
      p <- people_by_stratum[[key]]
      r <- rr_by_stratum[[key]]
      population_n <- sum(p$weight)
      exposed <- p$excess_units > 0
      excess_sum <- if (any(exposed)) {
        rr_power <- exp(outer(p$excess_units[exposed], log(r$rr_per_5)))
        colSums(p$weight[exposed] * (rr_power - 1))
      } else {
        rep(0, nrow(r))
      }
      data.frame(
        outcome = r$outcome,
        sex = r$sex,
        age_band = r$age_band,
        population_n = population_n,
        mean_rr = 1 + excess_sum / population_n,
        stringsAsFactors = FALSE
      )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(outcome, sex, age_band) |>
    dplyr::mutate(paf_obesity = pmax((mean_rr - 1) / mean_rr, 0))

  if (nrow(paf_by_stratum) == 0L) {
    stop(
      "No GBD relative-risk rows matched the simulated age-sex strata.",
      call. = FALSE
    )
  }

  paf_by_outcome <- paf_by_stratum |>
    dplyr::group_by(outcome) |>
    dplyr::summarise(
      paf_obesity = stats::weighted.mean(paf_obesity, w = population_n),
      .groups = "drop"
    ) |>
    dplyr::arrange(outcome)

  list(
    paf_by_stratum = paf_by_stratum,
    paf_by_outcome = paf_by_outcome
  )
}
