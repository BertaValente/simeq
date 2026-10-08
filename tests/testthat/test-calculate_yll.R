test_that(
  "simeq_calculate_yll calculates individual YLL correctly",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_1",
        "person_2",
        "person_3",
        "person_4"
      ),
      simulation = c(1L, 1L, 1L, 1L, 1L),
      year = c(2020L, 2021L, 2020L, 2020L, 2020L),
      age = c(59L, 60L, 50L, 80L, 85L),
      sex = c("women", "women", "men", "men", "women"),
      education_level = c("high", "high", "medium", "low", "high"),
      survey_weight = c(100, 100, 200, 50, 75),
      alive = c(TRUE, FALSE, TRUE, FALSE, FALSE),
      death_year = c(NA_integer_, 2021L, NA_integer_, 2020L, 2020L),
      age_at_death = c(NA_integer_, 60L, NA_integer_, 80L, 85L),
      stringsAsFactors = FALSE
    )

    results <- simeq_calculate_yll(
      simulation_output = simulation_output,
      ref_age = 75L,
      min_age = 20L,
      max_age = 84L
    )

    individual <- results$individual

    expect_equal(nrow(individual), 4L)

    expect_equal(
      unname(individual$yll),
      c(15, 0, 0, 0)
    )

    expect_equal(
      unname(individual$died),
      c(TRUE, FALSE, TRUE, TRUE)
    )
  }
)

test_that(
  "simeq_calculate_yll calculates weighted YLL totals by death year",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2",
        "person_3"
      ),
      simulation = c(1L, 1L, 1L),
      year = c(2020L, 2020L, 2021L),
      age = c(60L, 70L, 80L),
      sex = c("women", "women", "women"),
      education_level = c("high", "high", "high"),
      survey_weight = c(100, 50, 20),
      alive = c(FALSE, FALSE, FALSE),
      death_year = c(2020L, 2020L, 2021L),
      age_at_death = c(60L, 70L, 80L),
      stringsAsFactors = FALSE
    )

    results <- simeq_calculate_yll(
      simulation_output = simulation_output,
      ref_age = 75L,
      min_age = 20L,
      max_age = 84L
    )

    by_year_sim <- results$by_year_sim

    yll_2020 <- by_year_sim[
      by_year_sim$death_year == 2020L,
      ,
      drop = FALSE
    ]

    expect_equal(yll_2020$total_yll_weighted, 1750)

    yll_2021 <- by_year_sim[
      by_year_sim$death_year == 2021L,
      ,
      drop = FALSE
    ]

    expect_equal(yll_2021$total_yll_weighted, 0)
  }
)

test_that("simeq_calculate_yll uses remaining life expectancy when supplied", {
  simulation_output <- data.frame(
    person_id = c("person_1", "person_2"),
    simulation = c(1L, 1L),
    sex = c("men", "women"),
    education_level = c("low", "high"),
    survey_weight = c(10, 10),
    death_year = c(2020L, 2020L),
    age_at_death = c(80L, 40L),
    stringsAsFactors = FALSE
  )

  ref_life_expectancy <- expand.grid(
    age = 20:84,
    sex = c("men", "women"),
    stringsAsFactors = FALSE
  )
  ref_life_expectancy$ex <- 90 - ref_life_expectancy$age

  result <- simeq_calculate_yll(
    simulation_output,
    ref_age = c(men = 77L, women = 81L),
    ref_life_expectancy = ref_life_expectancy
  )

  # Man dying at 80 still loses 10 years, instead of 0 with ref_age 77.
  expect_equal(result$individual$yll, c(10, 50))
})
