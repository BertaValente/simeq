make_state_for_demographic_update <- function() {
  population <- data.frame(
    person_id = c(
      "person_1",
      "person_2",
      "person_3"
    ),
    age = c(
      35L,
      83L,
      84L
    ),
    sex = c(
      "women",
      "men",
      "women"
    ),
    education_level = c(
      "high",
      "medium",
      "low"
    ),
    bmi = c(
      23.5,
      27.1,
      31.2
    ),
    survey_weight = c(
      1000,
      1200,
      900
    )
  )

  ponte_initialise_population(
    population = population,
    settings = ponte_settings()
  )
}

test_that(
  "ponte_update_demography ages the state and advances the year",
  {
    state <- make_state_for_demographic_update()

    updated_state <- ponte_update_demography(state)

    expect_s3_class(
      updated_state,
      "data.frame"
    )

    expect_identical(
      updated_state$person_id,
      c(
        "person_1",
        "person_2",
        "person_3"
      )
    )

    expect_identical(
      updated_state$age,
      c(
        36L,
        84L,
        84L
      )
    )

    expect_identical(
      updated_state$year,
      c(
        2021L,
        2021L,
        2021L
      )
    )

    expect_identical(
      updated_state$baseline_year,
      c(
        2020L,
        2020L,
        2020L
      )
    )
  }
)

test_that(
  "ponte_update_demography retains non-demographic state values",
  {
    state <- make_state_for_demographic_update()

    updated_state <- ponte_update_demography(state)

    unchanged_columns <- setdiff(
      names(state),
      c(
        "age",
        "year"
      )
    )

    expect_identical(
      updated_state[unchanged_columns],
      state[unchanged_columns]
    )
  }
)

test_that(
  "ponte_update_demography validates its input state",
  {
    state <- make_state_for_demographic_update()

    state$alive[1] <- FALSE

    expect_error(
      ponte_update_demography(state),
      "`state` must contain only living records"
    )
  }
)
