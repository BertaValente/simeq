make_state_for_apply_mortality <- function() {
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
      "medium"
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

make_probabilities_for_apply_mortality <- function(
    state,
    probabilities = c(
      0,
      1
    )
) {
  data.frame(
    person_id = state$person_id,
    simulation = state$simulation,
    p_die = probabilities,
    stringsAsFactors = FALSE
  )
}

test_that(
  "simeq_apply_mortality separates deterministic deaths and survivors",
  {
    state <- make_state_for_apply_mortality()

    mortality_probabilities <- make_probabilities_for_apply_mortality(
      state = state,
      probabilities = c(
        0,
        1
      )
    )

    result <- simeq_apply_mortality(
      state = state,
      mortality_probabilities = mortality_probabilities
    )

    expect_named(
      result,
      c(
        "living_state",
        "deaths"
      )
    )

    expect_identical(
      result$living_state$person_id,
      "person_1"
    )

    expect_identical(
      result$deaths$person_id,
      "person_2"
    )
  }
)

test_that(
  "simeq_apply_mortality records death status year and age",
  {
    state <- make_state_for_apply_mortality()

    mortality_probabilities <- make_probabilities_for_apply_mortality(
      state = state,
      probabilities = c(
        0,
        1
      )
    )

    result <- simeq_apply_mortality(
      state = state,
      mortality_probabilities = mortality_probabilities
    )

    expect_false(
      result$deaths$alive
    )

    expect_identical(
      result$deaths$death_year,
      2021L
    )

    expect_identical(
      result$deaths$age_at_death,
      52L
    )

    expect_true(
      result$living_state$alive
    )

    expect_true(
      is.na(
        result$living_state$death_year
      )
    )

    expect_true(
      is.na(
        result$living_state$age_at_death
      )
    )
  }
)

test_that(
  "simeq_apply_mortality respects an explicit death year",
  {
    state <- make_state_for_apply_mortality()

    mortality_probabilities <- make_probabilities_for_apply_mortality(
      state = state,
      probabilities = c(
        0,
        1
      )
    )

    result <- simeq_apply_mortality(
      state = state,
      mortality_probabilities = mortality_probabilities,
      death_year = 2025L
    )

    expect_identical(
      result$deaths$death_year,
      2025L
    )
  }
)

test_that(
  "simeq_apply_mortality retains all records when probabilities are zero",
  {
    state <- make_state_for_apply_mortality()

    mortality_probabilities <- make_probabilities_for_apply_mortality(
      state = state,
      probabilities = c(
        0,
        0
      )
    )

    result <- simeq_apply_mortality(
      state = state,
      mortality_probabilities = mortality_probabilities
    )

    expect_identical(
      result$living_state,
      state
    )

    expect_identical(
      nrow(result$deaths),
      0L
    )
  }
)

test_that(
  "simeq_apply_mortality requires one probability for every person",
  {
    state <- make_state_for_apply_mortality()

    mortality_probabilities <- make_probabilities_for_apply_mortality(
      state = state,
      probabilities = c(
        0,
        1
      )
    )

    mortality_probabilities <- mortality_probabilities[1, ]

    expect_error(
      simeq_apply_mortality(
        state = state,
        mortality_probabilities = mortality_probabilities
      ),
      "`mortality_probabilities` must contain exactly one matching record"
    )
  }
)

test_that(
  "simeq_apply_mortality rejects invalid death probabilities",
  {
    state <- make_state_for_apply_mortality()

    mortality_probabilities <- make_probabilities_for_apply_mortality(
      state = state,
      probabilities = c(
        0,
        1.1
      )
    )

    expect_error(
      simeq_apply_mortality(
        state = state,
        mortality_probabilities = mortality_probabilities
      ),
      "`p_die` must contain finite probabilities from 0 to 1"
    )
  }
)
