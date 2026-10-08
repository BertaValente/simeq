test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
make_living_state_for_validation <- function() {
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
  "simeq_validate_living_state accepts a valid living state",
  {
    state <- make_living_state_for_validation()

    expect_invisible(
      simeq_validate_living_state(state)
    )
  }
)

test_that(
  "simeq_validate_living_state requires living records",
  {
    state <- make_living_state_for_validation()

    state$alive[2] <- FALSE

    expect_error(
      simeq_validate_living_state(state),
      "`state` must contain only living records"
    )
  }
)

test_that(
  "simeq_validate_living_state requires missing death information",
  {
    state <- make_living_state_for_validation()

    state$death_year[1] <- 2021L

    expect_error(
      simeq_validate_living_state(state),
      "`death_year` must be missing for living records"
    )

    state <- make_living_state_for_validation()

    state$age_at_death[1] <- 35L

    expect_error(
      simeq_validate_living_state(state),
      "`age_at_death` must be missing for living records"
    )
  }
)

test_that(
  "simeq_validate_living_state checks BMI-derived variables",
  {
    state <- make_living_state_for_validation()

    state$bmi_category[1] <- "obesity"

    expect_error(
      simeq_validate_living_state(state),
      "`bmi_category` must match the BMI values"
    )

    state <- make_living_state_for_validation()

    state$obesity[1] <- TRUE

    expect_error(
      simeq_validate_living_state(state),
      "`obesity` must be logical and match the BMI values"
    )
  }
)

test_that(
  "simeq_validate_living_state requires unique IDs per simulation",
  {
    state <- make_living_state_for_validation()

    state$person_id[2] <- state$person_id[1]

    expect_error(
      simeq_validate_living_state(state),
      "Each `person_id` must be unique within each simulation"
    )
  }
)
