make_state_for_ageing <- function() {
  population <- data.frame(
    person_id = c(
      "person_1",
      "person_2"
    ),
    age = c(
      35L,
      84L
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

  ponte_initialise_population(
    population = population,
    settings = ponte_settings()
  )
}

test_that(
  "ponte_age_population advances ages below the upper cap",
  {
    state <- make_state_for_ageing()

    aged_state <- ponte_age_population(state)

    expect_identical(
      aged_state$age[1],
      36L
    )
  }
)

test_that(
  "ponte_age_population retains people at the upper age cap",
  {
    state <- make_state_for_ageing()

    aged_state <- ponte_age_population(state)

    expect_identical(
      aged_state$age[2],
      84L
    )

    expect_identical(
      nrow(aged_state),
      2L
    )
  }
)

test_that(
  "ponte_age_population retains all non-age state values",
  {
    state <- make_state_for_ageing()

    aged_state <- ponte_age_population(state)

    unchanged_columns <- setdiff(
      names(state),
      "age"
    )

    expect_identical(
      aged_state[unchanged_columns],
      state[unchanged_columns]
    )
  }
)

test_that(
  "ponte_age_population validates its input state",
  {
    state <- make_state_for_ageing()

    state$alive[1] <- FALSE

    expect_error(
      ponte_age_population(state),
      "`state` must contain only living records"
    )
  }
)
