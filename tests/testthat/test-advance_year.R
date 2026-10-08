test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
make_state_for_advance_year <- function() {
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

test_that(
  "simeq_advance_year advances every record by one year",
  {
    state <- make_state_for_advance_year()

    updated_state <- simeq_advance_year(state)

    expect_identical(
      updated_state$year,
      c(2021L, 2021L)
    )

    expect_identical(
      updated_state$baseline_year,
      c(2020L, 2020L)
    )
  }
)

test_that(
  "simeq_advance_year retains all non-year state values",
  {
    state <- make_state_for_advance_year()

    updated_state <- simeq_advance_year(state)

    unchanged_columns <- setdiff(
      names(state),
      "year"
    )

    expect_identical(
      updated_state[unchanged_columns],
      state[unchanged_columns]
    )
  }
)

test_that(
  "simeq_advance_year validates its input state",
  {
    state <- make_state_for_advance_year()

    state$alive[1] <- FALSE

    expect_error(
      simeq_advance_year(state),
      "`state` must contain only living records"
    )
  }
)
