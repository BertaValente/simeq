test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
make_valid_population <- function() {
  data.frame(
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
}

test_that(
  "simeq_validate_population accepts a valid population",
  {
    population <- make_valid_population()

    expect_silent(
      simeq_validate_population(
        population = population,
        initial_year = 2020L
      )
    )
  }
)

test_that(
  "simeq_validate_population requires a data frame",
  {
    expect_error(
      simeq_validate_population(
        population = list(),
        initial_year = 2020L
      ),
      "`population` must be a data frame"
    )
  }
)

test_that(
  "simeq_validate_population checks required columns",
  {
    population <- make_valid_population()
    population$bmi <- NULL

    expect_error(
      simeq_validate_population(
        population = population,
        initial_year = 2020L
      ),
      "missing required columns: bmi"
    )
  }
)

test_that(
  "simeq_validate_population checks unique person IDs",
  {
    population <- make_valid_population()
    population$person_id[2] <- "person_1"

    expect_error(
      simeq_validate_population(
        population = population,
        initial_year = 2020L
      ),
      "`person_id` must be unique"
    )
  }
)

test_that(
  "simeq_validate_population checks age limits",
  {
    population <- make_valid_population()
    population$age[1] <- 19L

    expect_error(
      simeq_validate_population(
        population = population,
        initial_year = 2020L,
        entry_age = 20L,
        max_age = 84L
      ),
      "`age` must be between"
    )
  }
)

test_that(
  "simeq_validate_population checks BMI limits",
  {
    population <- make_valid_population()
    population$bmi[1] <- 85

    expect_error(
      simeq_validate_population(
        population = population,
        initial_year = 2020L
      ),
      "`bmi` values must be between 10 and 80"
    )
  }
)

test_that(
  "simeq_validate_population checks positive weights",
  {
    population <- make_valid_population()
    population$survey_weight[1] <- 0

    expect_error(
      simeq_validate_population(
        population = population,
        initial_year = 2020L
      ),
      "`survey_weight` must contain finite, positive"
    )
  }
)
