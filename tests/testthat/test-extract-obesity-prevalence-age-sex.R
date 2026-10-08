library(testthat)

make_sim_output <- function() {
  ages <- 20:84
  d <- expand.grid(
    year = 2025L, age = ages, sex = c("men", "women"),
    KEEP.OUT.ATTRS = FALSE
  )
  # Use BMI column (function now requires bmi, not obesity flag)
  d$bmi         <- ifelse(d$age >= 50L, 32, 23)  # obese if age >= 50
  d$alive       <- TRUE
  d$survey_weight <- 1
  d
}

test_that(
  "extract_simulated_obesity_prevalence_by_age_sex returns correct columns and strata",
  {
    result <- extract_simulated_obesity_prevalence_by_age_sex(
      simulation_output = make_sim_output(),
      year = 2025L
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 26L)
    expect_named(
      result,
      c("year", "sex", "age_band", "population_n", "p_overweight", "p_obesity")
    )
    expect_true(all(result$population_n == 5))
  }
)

test_that(
  "extract_simulated_obesity_prevalence_by_age_sex p_obesity matches expected values",
  {
    result <- extract_simulated_obesity_prevalence_by_age_sex(
      simulation_output = make_sim_output(),
      year = 2025L
    )

    # Ages 50+ have BMI = 32 → obese; ages < 50 have BMI = 23 → not obese
    under_50 <- result |> dplyr::filter(age_band %in% c("20-24","30-34","40-44"))
    over_50  <- result |> dplyr::filter(age_band %in% c("50-54","60-64","70-74"))

    expect_true(all(under_50$p_obesity == 0))
    expect_true(all(over_50$p_obesity  == 1))
  }
)
