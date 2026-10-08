make_population_for_initialisation <- function() {
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
    education_level2 = c(
      "higher",
      "lower"
    ),
    region_nuts = c(
      "PT11",
      "PT17"
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

test_that("simeq_initialise_population preserves BMI exactly", {

  population <- data.frame(
    person_id = c("p1", "p2", "p3"),
    age = c(20L, 45L, 84L),
    sex = c("women", "men", "women"),
    education_level = c("low", "medium", "high"),
    education_level2 = c("lower", "lower", "higher"),
    survey_weight = c(100, 200, 150),
    bmi = c(22.5, 27.8, 31.2),
    height = c(165, 175, 155),
    weight = c(61.3, 85.1, 75.0),
    stringsAsFactors = FALSE
  )

  settings <- simeq_settings(
    initial_year = 1998L,
    projection_end_year = 2000L,
    entry_age = 20L,
    max_age = 84L,
    n_simulations = 1L,
    entrant_n_per_year = 2L,
    seed = 123L
  )

  initial_state <- simeq_initialise_population(
    population = population,
    settings = settings
  )

  expect_equal(
    initial_state$bmi,
    population$bmi,
    tolerance = 1e-12
  )

  expect_equal(
    initial_state$survey_weight,
    population$survey_weight,
    tolerance = 1e-12
  )

  expect_equal(
    initial_state$age,
    population$age
  )

  expect_equal(
    initial_state$sex,
    population$sex
  )

  expect_equal(
    initial_state$education_level,
    population$education_level
  )

  expect_equal(
    initial_state$education_level2,
    population$education_level2
  )

  expect_equal(
    initial_state$year,
    rep(1998L, nrow(population))
  )

  expect_true(all(initial_state$alive))
})
