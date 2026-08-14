test_that(
  "calculate_obesity_pafs_age_sex calculates weighted outcome PAFs",
  {
    prevalence_age_sex <- tibble::tibble(
      year = c(2025L, 2025L),
      sex = c("Males", "Females"),
      age_band = c("20-24", "20-24"),
      population_n = c(100, 300),
      p_obesity = c(0.20, 0.40)
    )

    rr_matrix <- tibble::tibble(
      disease = c("Example disease", "Example disease"),
      sex = c("Males", "Females"),
      age_band = c("20-24", "20-24"),
      rr_per_5 = c(1.5, 1.5)
    )

    result <- calculate_obesity_pafs_age_sex(
      rr_matrix = rr_matrix,
      prevalence_age_sex = prevalence_age_sex,
      bmi_reference = 22,
      bmi_obesity = 27
    )

    expected_male_paf <- (0.20 * (1.5 - 1)) /
      (1 + 0.20 * (1.5 - 1))

    expected_female_paf <- (0.40 * (1.5 - 1)) /
      (1 + 0.40 * (1.5 - 1))

    expected_weighted_paf <- (
      expected_male_paf * 100 +
        expected_female_paf * 300
    ) / 400

    expect_named(
      result,
      c("paf_by_stratum", "paf_by_outcome")
    )

    expect_equal(
      nrow(result$paf_by_stratum),
      2L
    )

    expect_equal(
      result$paf_by_outcome$paf_obesity,
      expected_weighted_paf,
      tolerance = 1e-12
    )
  }
)
