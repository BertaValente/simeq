make_state_for_death_probability <- function() {
  population <- data.frame(
    person_id = c(
      "person_1",
      "person_2"
    ),
    age = c(
      35L,
      52L
    ),
    sex = c(
      "women",
      "men"
    ),
    education_level = c(
      "high",
      "low"
    ),
    bmi = c(
      23.5,
      31.2
    ),
    survey_weight = c(
      1000,
      1200
    )
  )

  simeq_initialise_population(
    population = population,
    settings = simeq_settings()
  )
}

make_life_table_for_death_probability <- function() {
  life_table <- expand.grid(
    age = 20:84,
    sex = c(
      "men",
      "women"
    ),
    stringsAsFactors = FALSE
  )

  life_table$qx <- 0.10

  life_table
}

make_mortality_risks_for_death_probability <- function() {
  rr_bmi <- expand.grid(
    sex = c(
      "men",
      "women"
    ),
    bmi_category = c(
      "underweight",
      "normal_weight",
      "overweight",
      "obesity_I",
      "obesity_II",
      "obesity_III"
    ),
    stringsAsFactors = FALSE
  )

  rr_bmi$rr_bmi <- 1

  rr_bmi$rr_bmi[
    rr_bmi$sex == "men" &
      rr_bmi$bmi_category == "obesity_I"
  ] <- 2

  rr_education <- data.frame(
    education_level2 = c(
      "lower",
      "higher"
    ),
    rr_education = c(
      1,
      0.5
    )
  )

  list(
    rr_bmi = rr_bmi,
    rr_education = rr_education
  )
}

test_that(
  "simeq_calculate_death_probability returns one probability per person",
  {
    state <- make_state_for_death_probability()

    life_table <- make_life_table_for_death_probability()

    risks <- make_mortality_risks_for_death_probability()

    result <- simeq_calculate_death_probability(
      state = state,
      life_table = life_table,
      rr_bmi = risks$rr_bmi,
      rr_education = risks$rr_education,
      education_source = "map_3_to_2"
    )

    expect_s3_class(
      result,
      "data.frame"
    )

    expect_identical(
      names(result),
      c(
        "person_id",
        "simulation",
        "p_die"
      )
    )

    expect_identical(
      result$person_id,
      c(
        "person_1",
        "person_2"
      )
    )

    expect_identical(
      result$simulation,
      c(
        1L,
        1L
      )
    )

    expect_true(
      all(
        result$p_die >= 0 &
          result$p_die <= 1
      )
    )
  }
)

test_that(
  "simeq_calculate_death_probability applies risks on the hazard scale",
  {
    # This test verifies the recalibrated hazard-scale RR logic.
    # Two persons are placed in the SAME age-sex stratum (both women/35) so
    # that recalibration preserves the weighted-mean mortality at qx while
    # creating individual differences that reflect their relative risks.
    #
    # Person 1: rr_bmi = 1, rr_education = 0.5  → total_rr = 0.5
    # Person 2: rr_bmi = 1, rr_education = 1.0  → total_rr = 1.0
    # Equal weights → stratum mean_rr = 0.75
    # Calibrated individual hazards:
    #   h1 = (h_lt / 0.75) * 0.5;  h2 = (h_lt / 0.75) * 1.0
    # Person 2 has higher p_die than person 1, and the weighted mean
    # of their p_die values should match qx = 0.10.

    population_same_stratum <- data.frame(
      person_id        = c("person_1", "person_2"),
      age              = c(35L, 35L),
      sex              = c("women", "women"),
      education_level  = c("high", "low"),
      bmi              = c(23.5, 23.5),   # both normal_weight → rr_bmi = 1
      survey_weight    = c(1000, 1000)
    )

    state_same_stratum <- simeq_initialise_population(
      population = population_same_stratum,
      settings   = simeq_settings()
    )

    life_table <- make_life_table_for_death_probability()

    # All rr_bmi = 1; rr_education differs
    rr_bmi_all_one <- expand.grid(
      sex          = c("men", "women"),
      bmi_category = c("underweight", "normal_weight", "overweight",
                       "obesity_I", "obesity_II", "obesity_III"),
      stringsAsFactors = FALSE
    )
    rr_bmi_all_one$rr_bmi <- 1

    rr_education_diff <- data.frame(
      education_level2 = c("lower", "higher"),
      rr_education     = c(1.0, 0.5)
    )

    result <- simeq_calculate_death_probability(
      state            = state_same_stratum,
      life_table       = life_table,
      rr_bmi           = rr_bmi_all_one,
      rr_education     = rr_education_diff,
      education_source = "map_3_to_2"
    )

    # Person 1 (rr=0.5) should have LOWER p_die than person 2 (rr=1.0)
    expect_true(result$p_die[1] < result$p_die[2])

    # Weighted mean p_die should be close to life-table qx = 0.10
    # (exact equality holds only in the hazard domain, so allow tolerance)
    w       <- population_same_stratum$survey_weight
    mean_pd <- sum(w * result$p_die) / sum(w)
    expect_equal(mean_pd, 0.10, tolerance = 0.01)
  }
)

test_that(
  "simeq_calculate_death_probability uses life-table qx when risks are one",
  {
    state <- make_state_for_death_probability()

    life_table <- make_life_table_for_death_probability()

    rr_bmi <- expand.grid(
      sex = c(
        "men",
        "women"
      ),
      bmi_category = c(
        "underweight",
        "normal_weight",
        "overweight",
        "obesity_I",
        "obesity_II",
        "obesity_III"
      ),
      stringsAsFactors = FALSE
    )

    rr_bmi$rr_bmi <- 1

    rr_education <- data.frame(
      education_level2 = c(
        "lower",
        "higher"
      ),
      rr_education = c(
        1,
        1
      )
    )

    result <- simeq_calculate_death_probability(
      state = state,
      life_table = life_table,
      rr_bmi = rr_bmi,
      rr_education = rr_education,
      education_source = "map_3_to_2"
    )

    expect_equal(
      result$p_die,
      c(
        0.10,
        0.10
      )
    )
  }
)

test_that(
  "simeq_calculate_death_probability supports existing education_level2",
  {
    state <- make_state_for_death_probability()

    state <- simeq_prepare_education_level2(
      state = state,
      source = "map_3_to_2"
    )

    life_table <- make_life_table_for_death_probability()

    risks <- make_mortality_risks_for_death_probability()

    result <- simeq_calculate_death_probability(
      state = state,
      life_table = life_table,
      rr_bmi = risks$rr_bmi,
      rr_education = risks$rr_education,
      education_source = "existing"
    )

    expect_identical(
      nrow(result),
      2L
    )
  }
)

test_that(
  "simeq_calculate_death_probability validates the living state",
  {
    state <- make_state_for_death_probability()

    state$alive[1] <- FALSE

    life_table <- make_life_table_for_death_probability()

    risks <- make_mortality_risks_for_death_probability()

    expect_error(
      simeq_calculate_death_probability(
        state = state,
        life_table = life_table,
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education,
        education_source = "map_3_to_2"
      ),
      "`state` must contain only living records"
    )
  }
)

test_that("fixed calibration lets obesity change the number of deaths", {
  make_state <- function(bmi) {
    population <- data.frame(
      person_id = paste0("person_", 1:2),
      age = c(50L, 50L),
      sex = c("men", "men"),
      education_level = c("high", "high"),
      bmi = bmi,
      survey_weight = c(1, 1)
    )
    simeq_initialise_population(population, simeq_settings())
  }

  life_table <- expand.grid(age = 20:84, sex = c("men", "women"),
                            stringsAsFactors = FALSE)
  life_table$qx <- 0.01

  rr_bmi <- expand.grid(
    sex = c("men", "women"),
    bmi_category = c("underweight", "normal_weight", "overweight",
                     "obesity_I", "obesity_II", "obesity_III"),
    stringsAsFactors = FALSE
  )
  rr_bmi$rr_bmi <- ifelse(rr_bmi$bmi_category == "obesity_I", 2, 1)

  rr_education <- data.frame(education_level2 = c("lower", "higher"),
                             rr_education = c(1, 1))

  baseline <- make_state(c(23, 23))
  heavier <- make_state(c(32, 32))

  calibration <- simeq_mortality_calibration_rr(
    baseline, rr_bmi, rr_education, education_source = "map_3_to_2"
  )

  annual <- simeq_calculate_death_probability(
    heavier, life_table, rr_bmi, rr_education,
    education_source = "map_3_to_2"
  )
  fixed <- simeq_calculate_death_probability(
    heavier, life_table, rr_bmi, rr_education,
    education_source = "map_3_to_2",
    calibration_rr = calibration
  )

  expect_equal(annual$p_die, rep(0.01, 2), tolerance = 1e-12)
  expect_equal(fixed$p_die, rep(1 - (1 - 0.01)^2, 2), tolerance = 1e-12)
})

test_that("a life table with a year column uses the matching year", {
  life_table <- expand.grid(
    age = 20:84, sex = c("men", "women"), year = c(1998L, 2020L),
    stringsAsFactors = FALSE
  )
  life_table$qx <- ifelse(life_table$year == 1998L, 0.02, 0.01)

  expect_equal(unique(simeq_select_life_table_year(life_table, 2005L)$qx), 0.02)
  expect_equal(unique(simeq_select_life_table_year(life_table, 2030L)$qx), 0.01)
  expect_equal(unique(simeq_select_life_table_year(life_table, 1990L)$qx), 0.02)
  expect_silent(simeq_validate_life_table(life_table))
})

test_that("education relative risk can be given per year of schooling", {
  population <- data.frame(
    person_id = paste0("person_", 1:2),
    age = c(40L, 40L),
    sex = c("women", "women"),
    education_level = c("low", "high"),
    education_years = c(6, 16),
    bmi = c(23, 23),
    survey_weight = c(1, 1)
  )
  state <- simeq_initialise_population(population, simeq_settings())

  life_table <- expand.grid(age = 20:84, sex = c("men", "women"),
                            stringsAsFactors = FALSE)
  life_table$qx <- 0.01

  rr_bmi <- expand.grid(
    sex = c("men", "women"),
    bmi_category = c("underweight", "normal_weight", "overweight",
                     "obesity_I", "obesity_II", "obesity_III"),
    stringsAsFactors = FALSE
  )
  rr_bmi$rr_bmi <- 1

  # IHME-CHAIN 2024: 1.9% lower mortality per additional year of schooling.
  rr_education <- data.frame(rr_per_year = 0.981)

  result <- simeq_calculate_death_probability(
    state, life_table, rr_bmi, rr_education,
    education_source = "map_3_to_2"
  )

  hazard <- -log(1 - result$p_die)
  expect_equal(hazard[2] / hazard[1], 0.981^10, tolerance = 1e-10)

  age_specific <- data.frame(
    age_min = c(20L, 50L),
    age_max = c(49L, 84L),
    rr_per_year = c(0.971, 0.992)
  )
  expect_silent(simeq_validate_mortality_risks(rr_bmi, age_specific))

  result_age <- simeq_calculate_death_probability(
    state, life_table, rr_bmi, age_specific,
    education_source = "map_3_to_2"
  )
  hazard_age <- -log(1 - result_age$p_die)
  expect_equal(hazard_age[2] / hazard_age[1], 0.971^10, tolerance = 1e-10)

  expect_error(
    simeq_validate_mortality_risks(
      rr_bmi,
      data.frame(rr_per_year = c(0.97, 0.99))
    ),
    "age_min"
  )
})
