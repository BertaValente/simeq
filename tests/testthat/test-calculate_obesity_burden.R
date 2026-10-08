test_that(
  "simeq_calculate_obesity_burden calculates PAF, YLD and DALYs",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2",
        "person_3"
      ),
      simulation = c(1L, 1L, 1L),
      year = c(2020L, 2020L, 2020L),
      age = c(50L, 50L, 60L),
      sex = c("women", "women", "women"),
      education_level = c("high", "high", "high"),
      survey_weight = c(100, 100, 50),
      bmi = c(22, 32, 32),
      bmi_category = c(
        "normal_weight",
        "obesityclassI",
        "obesity_I"
      ),
      alive = c(TRUE, TRUE, FALSE),
      death_year = c(
        NA_integer_,
        NA_integer_,
        2020L
      ),
      age_at_death = c(
        NA_integer_,
        NA_integer_,
        60L
      ),
      stringsAsFactors = FALSE
    )

    rr_bmi <- data.frame(
      sex = c("women", "women"),
      bmi_category = c(
        "normal_weight",
        "obesity_I"
      ),
      rr_bmi = c(1, 2),
      stringsAsFactors = FALSE
    )

    results <- simeq_calculate_obesity_burden(
      simulation_output = simulation_output,
      rr_bmi = rr_bmi,
      disability_weight = 0.05,
      ref_age = 75L,
      min_age = 20L,
      max_age = 84L
    )

    burden <- results$by_year_sim

    expect_equal(burden$N_weighted, 200)

    expect_equal(
      burden$prev_obesity,
      0.5
    )

    expect_equal(
      burden$paf_obesity,
      1 / 3,
      tolerance = 1e-10
    )

    expect_equal(
      burden$yll_obesity_attrib_weighted,
      250,
      tolerance = 1e-10
    )

    expect_equal(
      burden$yld_obesity_attrib,
      5,
      tolerance = 1e-10
    )

    expect_equal(
      burden$daly_obesity_weighted,
      255,
      tolerance = 1e-10
    )
    expect_equal(
      burden$yll_obesity_per100k,
      125000,
      tolerance = 1e-10
    )

    expect_equal(
      burden$yld_obesity_per100k,
      2500,
      tolerance = 1e-10
    )

    expect_equal(
      burden$daly_obesity_per100k,
      127500,
      tolerance = 1e-10
    )
  }
)

test_that(
  "simeq_calculate_obesity_burden returns zero PAF and YLD without obesity",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2"
      ),
      simulation = c(1L, 1L),
      year = c(2020L, 2020L),
      age = c(40L, 55L),
      sex = c("men", "men"),
      education_level = c("medium", "medium"),
      survey_weight = c(100, 50),
      bmi = c(22, 27),
      bmi_category = c(
        "normal_weight",
        "overweight"
      ),
      alive = c(TRUE, TRUE),
      death_year = c(
        NA_integer_,
        NA_integer_
      ),
      age_at_death = c(
        NA_integer_,
        NA_integer_
      ),
      stringsAsFactors = FALSE
    )

    rr_bmi <- data.frame(
      sex = c("men", "men"),
      bmi_category = c(
        "normal_weight",
        "overweight"
      ),
      rr_bmi = c(1, 1.1),
      stringsAsFactors = FALSE
    )

    results <- simeq_calculate_obesity_burden(
      simulation_output = simulation_output,
      rr_bmi = rr_bmi
    )

    burden <- results$by_year_sim

    expect_equal(burden$paf_obesity, 0)

    expect_equal(
      burden$yld_obesity_attrib,
      0
    )

    expect_equal(
      burden$daly_obesity_weighted,
      0
    )
  }
)

test_that(
  "simeq_calculate_obesity_burden summarises burden-rate intervals",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2"
      ),
      simulation = c(1L, 2L),
      year = c(2020L, 2020L),
      age = c(40L, 40L),
      sex = c("men", "men"),
      education_level = c("medium", "medium"),
      survey_weight = c(100, 100),
      bmi = c(35, 35),
      bmi_category = c(
        "obesity_I",
        "obesity_I"
      ),
      alive = c(TRUE, TRUE),
      death_year = c(NA_integer_, NA_integer_),
      age_at_death = c(NA_integer_, NA_integer_),
      stringsAsFactors = FALSE
    )

    rr_bmi <- data.frame(
      sex = "men",
      bmi_category = "obesity_I",
      rr_bmi = 1.5,
      stringsAsFactors = FALSE
    )

    results <- simeq_calculate_obesity_burden(
      simulation_output = simulation_output,
      rr_bmi = rr_bmi
    )

    expect_true(
      all(
        c(
          "yll_obesity_per100k_lo",
          "yll_obesity_per100k_hi",
          "yld_obesity_per100k_lo",
          "yld_obesity_per100k_hi",
          "daly_obesity_per100k_lo",
          "daly_obesity_per100k_hi"
        ) %in% names(results$by_year)
      )
    )

    expect_equal(
      results$by_year$yld_obesity_per100k,
      5000,
      tolerance = 1e-10
    )

    expect_equal(
      results$by_year$yld_obesity_per100k_lo,
      5000,
      tolerance = 1e-10
    )

    expect_equal(
      results$by_year$yld_obesity_per100k_hi,
      5000,
      tolerance = 1e-10
    )
  }
)

test_that(
  "attributable YLL uses the PAF of the year before death",
  {
    simulation_output <- data.frame(
      person_id = c("p1", "p2", "p1", "p2", "p3"),
      simulation = 1L,
      year = c(2020L, 2020L, 2021L, 2021L, 2021L),
      age = c(50L, 50L, 51L, 51L, 60L),
      sex = "women",
      education_level = "high",
      survey_weight = c(100, 100, 100, 100, 50),
      bmi = c(32, 32, 22, 22, 32),
      bmi_category = c(
        "obesity_I", "obesity_I", "normal_weight", "normal_weight",
        "obesity_I"
      ),
      alive = c(TRUE, TRUE, TRUE, TRUE, FALSE),
      death_year = c(NA, NA, NA, NA, 2021L),
      age_at_death = c(NA, NA, NA, NA, 60L),
      stringsAsFactors = FALSE
    )

    rr_bmi <- data.frame(
      sex = c("women", "women"),
      bmi_category = c("normal_weight", "obesity_I"),
      rr_bmi = c(1, 2),
      stringsAsFactors = FALSE
    )

    burden <- simeq_calculate_obesity_burden(
      simulation_output = simulation_output,
      rr_bmi = rr_bmi,
      ref_age = 75L
    )$by_year_sim

    row_2021 <- burden[burden$year == 2021L, ]

    # 2021 survivors are not obese (PAF 0), but the 2020 population that
    # produced the death was all obese (PAF 0.5).
    expect_equal(row_2021$paf_obesity, 0)
    expect_equal(row_2021$yll_obesity_attrib_weighted, 15 * 50 * 0.5)
  }
)
