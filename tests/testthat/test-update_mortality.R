make_state_for_mortality_update <- function() {
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

make_life_table_for_mortality_update <- function(
    qx = 0
) {
  life_table <- expand.grid(
    age = 20:84,
    sex = c(
      "men",
      "women"
    ),
    stringsAsFactors = FALSE
  )

  life_table$qx <- qx

  life_table
}

make_risks_for_mortality_update <- function() {
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

  list(
    rr_bmi = rr_bmi,
    rr_education = rr_education
  )
}

test_that(
  "simeq_update_mortality returns survivors deaths and probabilities",
  {
    state <- make_state_for_mortality_update()

    life_table <- make_life_table_for_mortality_update(
      qx = 0
    )

    risks <- make_risks_for_mortality_update()

    result <- simeq_update_mortality(
      state = state,
      life_table = life_table,
      rr_bmi = risks$rr_bmi,
      rr_education = risks$rr_education,
      education_source = "map_3_to_2"
    )

    expect_named(
      result,
      c(
        "living_state",
        "deaths",
        "mortality_probabilities"
      )
    )

    expect_identical(
      result$living_state,
      state
    )

    expect_identical(
      nrow(result$deaths),
      0L
    )

    expect_identical(
      result$mortality_probabilities$p_die,
      c(
        0,
        0
      )
    )
  }
)

test_that(
  "simeq_update_mortality records certain deaths",
  {
    state <- make_state_for_mortality_update()

    life_table <- make_life_table_for_mortality_update(
      qx = 1
    )

    risks <- make_risks_for_mortality_update()

    result <- simeq_update_mortality(
      state = state,
      life_table = life_table,
      rr_bmi = risks$rr_bmi,
      rr_education = risks$rr_education,
      education_source = "map_3_to_2"
    )

    expect_identical(
      nrow(result$living_state),
      0L
    )

    expect_identical(
      result$deaths$person_id,
      c(
        "person_1",
        "person_2"
      )
    )

    expect_true(
      all(!result$deaths$alive)
    )

    expect_identical(
      result$deaths$death_year,
      c(
        2021L,
        2021L
      )
    )

    expect_identical(
      result$deaths$age_at_death,
      c(
        35L,
        52L
      )
    )

    expect_identical(
      result$mortality_probabilities$p_die,
      c(
        1,
        1
      )
    )
  }
)

test_that(
  "simeq_update_mortality supports existing education_level2",
  {
    state <- make_state_for_mortality_update()

    state <- simeq_prepare_education_level2(
      state = state,
      source = "map_3_to_2"
    )

    life_table <- make_life_table_for_mortality_update(
      qx = 0
    )

    risks <- make_risks_for_mortality_update()

    result <- simeq_update_mortality(
      state = state,
      life_table = life_table,
      rr_bmi = risks$rr_bmi,
      rr_education = risks$rr_education,
      education_source = "existing"
    )

    expect_identical(
      result$mortality_probabilities$p_die,
      c(
        0,
        0
      )
    )
  }
)

test_that(
  "simeq_update_mortality validates mortality inputs",
  {
    state <- make_state_for_mortality_update()

    life_table <- make_life_table_for_mortality_update(
      qx = 0
    )

    risks <- make_risks_for_mortality_update()

    risks$rr_bmi$rr_bmi[1] <- 0

    expect_error(
      simeq_update_mortality(
        state = state,
        life_table = life_table,
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education,
        education_source = "map_3_to_2"
      ),
      "`rr_bmi\\$rr_bmi` must contain positive, finite values"
    )
  }
)
