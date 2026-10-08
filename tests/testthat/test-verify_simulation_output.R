make_valid_simulation_output_for_verification <- function() {
  data.frame(
    person_id = c(
      "person_1",
      "person_1",
      "entrant_2021_1_1",
      "entrant_2021_1_2"
    ),
    simulation = c(
      1L,
      1L,
      1L,
      1L
    ),
    year = c(
      2020L,
      2021L,
      2021L,
      2021L
    ),
    baseline_year = c(
      2020L,
      2020L,
      2021L,
      2021L
    ),
    age = c(
      30L,
      31L,
      20L,
      20L
    ),
    sex = c(
      "men",
      "men",
      "men",
      "women"
    ),
    education_level = c(
      "medium",
      "medium",
      "low",
      "high"
    ),
    education_level2 = c(
      "lower",
      "lower",
      "lower",
      "higher"
    ),
    survey_weight = c(
      100,
      100,
      49000,
      48000
    ),
    bmi = c(
      25,
      26,
      22,
      24
    ),
    bmi_category = c(
      "overweight",
      "overweight",
      "normal_weight",
      "normal_weight"
    ),
    obesity = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),
    alive = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    death_year = c(
      NA_integer_,
      NA_integer_,
      NA_integer_,
      NA_integer_
    ),
    age_at_death = c(
      NA_integer_,
      NA_integer_,
      NA_integer_,
      NA_integer_
    ),
    stringsAsFactors = FALSE
  )
}

make_entrant_targets_for_verification <- function() {
  data.frame(
    entry_year = c(
      2021L,
      2021L
    ),
    sex = c(
      "men",
      "women"
    ),
    entrant_population = c(
      49000,
      48000
    ),
    stringsAsFactors = FALSE
  )
}

test_that(
  "simeq_verify_simulation_output passes valid output",
  {
    verification <- simeq_verify_simulation_output(
      simulation_output =
        make_valid_simulation_output_for_verification(),
      entrant_targets =
        make_entrant_targets_for_verification()
    )

    expect_s3_class(
      verification,
      "data.frame"
    )

    expect_identical(
      nrow(verification),
      12L
    )

    expect_true(
      all(
        verification$passed
      )
    )
  }
)

test_that(
  "simeq_verify_simulation_output detects duplicate person-years",
  {
    simulation_output <-
      make_valid_simulation_output_for_verification()

    simulation_output <- rbind(
      simulation_output,
      simulation_output[1L, , drop = FALSE]
    )

    verification <- simeq_verify_simulation_output(
      simulation_output = simulation_output
    )

    expect_false(
      verification$passed[
        verification$check ==
          "Person-year uniqueness"
      ]
    )
  }
)

test_that(
  "simeq_verify_simulation_output detects invalid death-year records",
  {
    simulation_output <-
      make_valid_simulation_output_for_verification()

    simulation_output$alive[2L] <- FALSE
    simulation_output$death_year[2L] <- 2020L
    simulation_output$age_at_death[2L] <- 31L

    verification <- simeq_verify_simulation_output(
      simulation_output = simulation_output
    )

    expect_false(
      verification$passed[
        verification$check ==
          "Death-year and death-age consistency"
      ]
    )
  }
)

test_that(
  "simeq_verify_simulation_output detects BMI-category mismatch",
  {
    simulation_output <-
      make_valid_simulation_output_for_verification()

    simulation_output$bmi_category[1L] <- "obesity"

    verification <- simeq_verify_simulation_output(
      simulation_output = simulation_output
    )

    expect_false(
      verification$passed[
        verification$check ==
          "BMI-category consistency"
      ]
    )
  }
)

test_that(
  "simeq_verify_simulation_output detects incorrect entrant age",
  {
    simulation_output <-
      make_valid_simulation_output_for_verification()

    simulation_output$age[3L] <- 21L

    verification <- simeq_verify_simulation_output(
      simulation_output = simulation_output
    )

    expect_false(
      verification$passed[
        verification$check ==
          "Entrants begin at entry age"
      ]
    )
  }
)

test_that(
  "simeq_verify_simulation_output detects entrant target mismatch",
  {
    entrant_targets <- make_entrant_targets_for_verification()

    entrant_targets$entrant_population[
      entrant_targets$sex == "men"
    ] <- 50000

    verification <- simeq_verify_simulation_output(
      simulation_output =
        make_valid_simulation_output_for_verification(),
      entrant_targets = entrant_targets
    )

    expect_false(
      verification$passed[
        verification$check ==
          "Entrant weights match targets"
      ]
    )
  }
)

test_that(
  "simeq_verify_simulation_output requires complete output columns",
  {
    simulation_output <-
      make_valid_simulation_output_for_verification()

    simulation_output$bmi <- NULL

    expect_error(
      simeq_verify_simulation_output(
        simulation_output = simulation_output
      ),
      "`simulation_output` is missing required columns: bmi.",
      fixed = TRUE
    )
  }
)
