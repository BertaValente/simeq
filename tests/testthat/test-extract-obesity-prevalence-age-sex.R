library(testthat)

test_that(
  "extract_simulated_obesity_prevalence_by_age_sex returns GBD age strata",
  {
    ages <- 20:84

    simulation_output <- expand.grid(
      year = 2025L,
      age = ages,
      sex = c("men", "women"),
      KEEP.OUT.ATTRS = FALSE
    )

    simulation_output$obesity <- simulation_output$age >= 50L
    simulation_output$alive <- TRUE
    simulation_output$surveyweight <- 1

    result <- extract_simulated_obesity_prevalence_by_age_sex(
      simulation_output = simulation_output,
      year = 2025L
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 26L)
    expect_named(
      result,
      c("year", "sex", "age_band", "population_n", "p_obesity")
    )
    expect_true(
      all(result$population_n == 5)
    )
    expect_equal(
      result |>
        dplyr::filter(
          sex == "Males",
          age_band == "45-49"
        ) |>
        dplyr::pull(p_obesity),
      0
    )
    expect_equal(
      result |>
        dplyr::filter(
          sex == "Females",
          age_band == "50-54"
        ) |>
        dplyr::pull(p_obesity),
      1
    )
  }
)
