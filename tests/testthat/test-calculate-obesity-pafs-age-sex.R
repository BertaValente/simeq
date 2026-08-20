make_prevalence <- function(p_overweight = NULL) {
  d <- tibble::tibble(
    year         = c(2025L, 2025L),
    sex          = c("Males", "Females"),
    age_band     = c("20-24", "20-24"),
    population_n = c(100, 300),
    p_obesity    = c(0.20, 0.40)
  )
  if (!is.null(p_overweight)) {
    d$p_overweight <- p_overweight
  }
  d
}

make_rr_matrix <- function() {
  tibble::tibble(
    disease  = c("Example disease", "Example disease"),
    sex      = c("Males", "Females"),
    age_band = c("20-24", "20-24"),
    rr_per_5 = c(1.5, 1.5)
  )
}

test_that(
  "calculate_obesity_pafs_age_sex calculates single-category (obesity-only) PAFs",
  {
    result <- calculate_obesity_pafs_age_sex(
      rr_matrix          = make_rr_matrix(),
      prevalence_age_sex = make_prevalence(),
      bmi_reference      = 22,
      bmi_obesity        = 27
    )

    # scale_factor = (27-22)/5 = 1 → rr_ob = 1.5^1 = 1.5
    expected_male_paf   <- (0.20 * (1.5 - 1)) / (1 + 0.20 * (1.5 - 1))
    expected_female_paf <- (0.40 * (1.5 - 1)) / (1 + 0.40 * (1.5 - 1))
    expected_weighted   <- (expected_male_paf * 100 +
                              expected_female_paf * 300) / 400

    expect_named(result, c("paf_by_stratum", "paf_by_outcome"))
    expect_equal(nrow(result$paf_by_stratum), 2L)
    expect_equal(result$paf_by_outcome$paf_obesity, expected_weighted,
                 tolerance = 1e-12)
  }
)

test_that(
  "calculate_obesity_pafs_age_sex calculates two-category (overweight + obesity) PAFs",
  {
    # scale_ob = (27-22)/5 = 1 → rr_ob = 1.5; scale_ow = (24-22)/5 = 0.4
    rr_ow <- 1.5 ^ 0.4
    rr_ob <- 1.5 ^ 1.0
    p_ow  <- 0.30
    p_ob  <- 0.20

    expected_paf <- (p_ow * (rr_ow - 1) + p_ob * (rr_ob - 1)) /
                   (1 + p_ow * (rr_ow - 1) + p_ob * (rr_ob - 1))

    # Both strata must have the same p_obesity so the single-formula expectation
    # covers both; make_prevalence() defaults to c(0.20, 0.40) which would need
    # a sex-specific weighted expected value.
    prevalence <- tibble::tibble(
      year = c(2025L, 2025L), sex = c("Males", "Females"),
      age_band = c("20-24", "20-24"), population_n = c(100, 300),
      p_obesity = c(p_ob, p_ob), p_overweight = c(p_ow, p_ow)
    )

    result <- calculate_obesity_pafs_age_sex(
      rr_matrix          = make_rr_matrix(),
      prevalence_age_sex = prevalence,
      bmi_reference      = 22,
      bmi_overweight     = 24,   # → scale = 0.4 → rr = 1.5^0.4
      bmi_obesity        = 27    # → scale = 1.0 → rr = 1.5
    )

    # Both strata identical (same p_ow / p_ob / rr), so weighted mean = expected_paf
    expect_equal(result$paf_by_outcome$paf_obesity, expected_paf,
                 tolerance = 1e-12)
  }
)

test_that(
  "calculate_obesity_pafs_age_sex errors if p_overweight missing with bmi_overweight",
  {
    expect_error(
      calculate_obesity_pafs_age_sex(
        rr_matrix          = make_rr_matrix(),
        prevalence_age_sex = make_prevalence(),   # no p_overweight column
        bmi_reference      = 22,
        bmi_overweight     = 24,
        bmi_obesity        = 27
      ),
      "p_overweight"
    )
  }
)

test_that(
  "calculate_obesity_pafs_age_sex clamps protective associations to zero",
  {
    # RR = 0.8 (protective) — PAF should be clamped to 0
    rr_matrix_protective <- tibble::tibble(
      disease  = "Protective disease",
      sex      = "Males",
      age_band = "20-24",
      rr_per_5 = 0.8
    )

    prevalence <- tibble::tibble(
      year         = 2025L,
      sex          = "Males",
      age_band     = "20-24",
      population_n = 100,
      p_obesity    = 0.20
    )

    result <- calculate_obesity_pafs_age_sex(
      rr_matrix          = rr_matrix_protective,
      prevalence_age_sex = prevalence,
      bmi_reference      = 22,
      bmi_obesity        = 27
    )

    expect_equal(result$paf_by_outcome$paf_obesity, 0)
  }
)
