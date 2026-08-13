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

  ponte_initialise_population(
    population = population,
    settings = ponte_settings()
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
      "obesity"
    ),
    stringsAsFactors = FALSE
  )

  rr_bmi$rr_bmi <- 1

  rr_bmi$rr_bmi[
    rr_bmi$sex == "men" &
      rr_bmi$bmi_category == "obesity"
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
  "ponte_calculate_death_probability returns one probability per person",
  {
    state <- make_state_for_death_probability()

    life_table <- make_life_table_for_death_probability()

    risks <- make_mortality_risks_for_death_probability()

    result <- ponte_calculate_death_probability(
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
  "ponte_calculate_death_probability applies risks on the hazard scale",
  {
    state <- make_state_for_death_probability()

    life_table <- make_life_table_for_death_probability()

    risks <- make_mortality_risks_for_death_probability()

    result <- ponte_calculate_death_probability(
      state = state,
      life_table = life_table,
      rr_bmi = risks$rr_bmi,
      rr_education = risks$rr_education,
      education_source = "map_3_to_2"
    )

    expected_person_1 <- 1 - (
      1 - 0.10
    )^0.5

    expected_person_2 <- 1 - (
      1 - 0.10
    )^2

    expect_equal(
      result$p_die,
      c(
        expected_person_1,
        expected_person_2
      )
    )
  }
)

test_that(
  "ponte_calculate_death_probability uses life-table qx when risks are one",
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
        "obesity"
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

    result <- ponte_calculate_death_probability(
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
  "ponte_calculate_death_probability supports existing education_level2",
  {
    state <- make_state_for_death_probability()

    state <- ponte_prepare_education_level2(
      state = state,
      source = "map_3_to_2"
    )

    life_table <- make_life_table_for_death_probability()

    risks <- make_mortality_risks_for_death_probability()

    result <- ponte_calculate_death_probability(
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
  "ponte_calculate_death_probability validates the living state",
  {
    state <- make_state_for_death_probability()

    state$alive[1] <- FALSE

    life_table <- make_life_table_for_death_probability()

    risks <- make_mortality_risks_for_death_probability()

    expect_error(
      ponte_calculate_death_probability(
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
