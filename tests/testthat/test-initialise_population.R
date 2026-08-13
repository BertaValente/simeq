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

test_that(
  "ponte_initialise_population creates a valid initial state",
  {
    population <- make_population_for_initialisation()

    state <- ponte_initialise_population(
      population = population,
      settings = ponte_settings()
    )

    expect_s3_class(
      state,
      "data.frame"
    )

    expect_identical(
      nrow(state),
      2L
    )

    expect_true(
      all(
        c(
          "person_id",
          "simulation",
          "year",
          "baseline_year",
          "age",
          "sex",
          "education_level",
          "education_level2",
          "survey_weight",
          "bmi",
          "bmi_category",
          "obesity",
          "alive",
          "death_year",
          "age_at_death"
        ) %in% names(state)
      )
    )

    expect_identical(
      state$simulation,
      c(1L, 1L)
    )

    expect_identical(
      state$year,
      c(2020L, 2020L)
    )

    expect_identical(
      state$baseline_year,
      c(2020L, 2020L)
    )

    expect_identical(
      state$bmi_category,
      c(
        "normal_weight",
        "obesity"
      )
    )

    expect_identical(
      state$obesity,
      c(
        FALSE,
        TRUE
      )
    )

    expect_true(
      all(state$alive)
    )

    expect_true(
      all(is.na(state$death_year))
    )

    expect_true(
      all(is.na(state$age_at_death))
    )
  }
)

test_that(
  "ponte_initialise_population retains extra population variables",
  {
    population <- make_population_for_initialisation()

    state <- ponte_initialise_population(
      population = population,
      settings = ponte_settings()
    )

    expect_identical(
      state$education_level2,
      c(
        "higher",
        "lower"
      )
    )

    expect_identical(
      state$region_nuts,
      c(
        "PT11",
        "PT17"
      )
    )
  }
)

test_that(
  "ponte_initialise_population requires ponte settings",
  {
    population <- make_population_for_initialisation()

    expect_error(
      ponte_initialise_population(
        population = population,
        settings = list()
      ),
      "`settings` must be a `ponte_settings` object"
    )
  }
)
