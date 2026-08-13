make_population_for_simulation_runner <- function() {
  data.frame(
    person_id = c(
      "person_1",
      "person_2"
    ),
    age = c(
      30L,
      84L
    ),
    sex = c(
      "men",
      "women"
    ),
    education_level = c(
      "medium",
      "high"
    ),
    education_level2 = c(
      "lower",
      "higher"
    ),
    bmi = c(
      25,
      31
    ),
    survey_weight = c(
      100,
      100
    ),
    stringsAsFactors = FALSE
  )
}

make_life_table_for_simulation_runner <- function() {
  life_table <- expand.grid(
    age = 20:84,
    sex = c(
      "men",
      "women"
    ),
    stringsAsFactors = FALSE
  )

  life_table$qx <- 0

  life_table
}

make_bmi_risks_for_simulation_runner <- function() {
  rr_bmi <- expand.grid(
    sex = c(
      "men",
      "women"
    ),
    bmi_category = c(
      "underweight",
      "normal_weight",
      "overweight",
      "obesity"
    ),
    stringsAsFactors = FALSE
  )

  rr_bmi$rr_bmi <- 1

  rr_bmi
}

make_education_risks_for_simulation_runner <- function() {
  data.frame(
    education_level2 = c(
      "lower",
      "higher"
    ),
    rr_education = c(
      1,
      1
    ),
    stringsAsFactors = FALSE
  )
}

make_donor_pool_for_simulation_runner <- function() {
  data.frame(
    sex = c(
      "men",
      "men",
      "women",
      "women"
    ),
    age = c(
      20L,
      24L,
      21L,
      23L
    ),
    education_level = c(
      "low",
      "medium",
      "medium",
      "high"
    ),
    bmi = c(
      22.1,
      27.5,
      23.8,
      31.0
    ),
    survey_weight = c(
      800,
      1000,
      900,
      1100
    ),
    stringsAsFactors = FALSE
  )
}

make_entrant_targets_for_simulation_runner <- function() {
  data.frame(
    entry_year = c(
      2021L,
      2021L,
      2022L,
      2022L
    ),
    sex = c(
      "men",
      "women",
      "men",
      "women"
    ),
    entrant_population = c(
      49000,
      48000,
      49500,
      48500
    ),
    stringsAsFactors = FALSE
  )
}

make_settings_for_simulation_runner <- function(
    n_simulations = 2L
) {
  ponte_settings(
    initial_year = 2020L,
    projection_end_year = 2022L,
    entry_age = 20L,
    max_age = 84L,
    n_simulations = n_simulations,
    entrant_n_per_year = 100L,
    seed = 123L
  )
}

run_test_simulation <- function(
    n_simulations = 2L
) {
  bmi_transition_model <- ponte_fit_bmi_transition(
    bmi_transitions = make_bmi_transitions_for_model_fit()
  )

  ponte_run_simulation(
    population = make_population_for_simulation_runner(),
    settings = make_settings_for_simulation_runner(
      n_simulations = n_simulations
    ),
    life_table = make_life_table_for_simulation_runner(),
    rr_bmi = make_bmi_risks_for_simulation_runner(),
    rr_education = make_education_risks_for_simulation_runner(),
    bmi_transition_model = bmi_transition_model,
    donor_pool = make_donor_pool_for_simulation_runner(),
    entrant_targets = make_entrant_targets_for_simulation_runner(),
    education_source = "existing"
  )
}

test_that(
  "ponte_run_simulation returns all years and simulations",
  {
    simulation_output <- run_test_simulation(
      n_simulations = 2L
    )

    expect_s3_class(
      simulation_output,
      "data.frame"
    )

    expect_identical(
      sort(
        unique(
          simulation_output$simulation
        )
      ),
      c(
        1L,
        2L
      )
    )

    expect_identical(
      sort(
        unique(
          simulation_output$year
        )
      ),
      c(
        2020L,
        2021L,
        2022L
      )
    )

    expect_true(
      all(
        c(
          "education_level",
          "education_level2"
        ) %in%
          names(simulation_output)
      )
    )
  }
)

test_that(
  "ponte_run_simulation retains baseline records",
  {
    simulation_output <- run_test_simulation(
      n_simulations = 2L
    )

    baseline_records <- simulation_output[
      simulation_output$year == 2020L,
      ,
      drop = FALSE
    ]

    expect_identical(
      nrow(baseline_records),
      4L
    )

    expect_true(
      all(
        baseline_records$alive
      )
    )

    expect_identical(
      sort(
        unique(
          baseline_records$age
        )
      ),
      c(
        30L,
        84L
      )
    )
  }
)

test_that(
  "ponte_run_simulation creates annual age-20 entrants",
  {
    simulation_output <- run_test_simulation(
      n_simulations = 2L
    )

    entrants <- simulation_output[
      grepl(
        "^entrant_",
        simulation_output$person_id
      ) &
        simulation_output$year ==
        simulation_output$baseline_year,
      ,
      drop = FALSE
    ]

    entrant_counts <- aggregate(
      person_id ~ simulation + year,
      data = entrants,
      FUN = length
    )

    expect_identical(
      entrant_counts$person_id,
      c(
        100L,
        100L,
        100L,
        100L
      )
    )

    expect_true(
      all(
        entrants$age == 20L
      )
    )

    expect_true(
      all(
        entrants$year %in%
          c(
            2021L,
            2022L
          )
      )
    )
  }
)

test_that(
  "ponte_run_simulation matches annual entrant-weight targets",
  {
    simulation_output <- run_test_simulation(
      n_simulations = 1L
    )

    entrants <- simulation_output[
      grepl(
        "^entrant_",
        simulation_output$person_id
      ) &
        simulation_output$year ==
        simulation_output$baseline_year,
      ,
      drop = FALSE
    ]

    entrant_weights <- aggregate(
      survey_weight ~ year + sex,
      data = entrants,
      FUN = sum
    )

    expected_weights <- make_entrant_targets_for_simulation_runner()

    matched_weights <- merge(
      x = entrant_weights,
      y = expected_weights,
      by.x = c(
        "year",
        "sex"
      ),
      by.y = c(
        "entry_year",
        "sex"
      ),
      all = TRUE
    )

    expect_equal(
      matched_weights$survey_weight,
      matched_weights$entrant_population
    )
  }
)

test_that(
  "ponte_run_simulation produces unique person-year records",
  {
    simulation_output <- run_test_simulation(
      n_simulations = 2L
    )

    person_year_identifier <- paste(
      simulation_output$simulation,
      simulation_output$person_id,
      simulation_output$year,
      sep = "::"
    )

    expect_identical(
      anyDuplicated(
        person_year_identifier
      ),
      0L
    )
  }
)

test_that(
  "ponte_run_simulation is reproducible from settings seed",
  {
    output_one <- run_test_simulation(
      n_simulations = 1L
    )

    output_two <- run_test_simulation(
      n_simulations = 1L
    )

    expect_identical(
      output_one,
      output_two
    )
  }
)

test_that(
  "ponte_run_simulation requires ponte settings",
  {
    bmi_transition_model <- ponte_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    expect_error(
      ponte_run_simulation(
        population = make_population_for_simulation_runner(),
        settings = list(),
        life_table = make_life_table_for_simulation_runner(),
        rr_bmi = make_bmi_risks_for_simulation_runner(),
        rr_education = make_education_risks_for_simulation_runner(),
        bmi_transition_model = bmi_transition_model,
        donor_pool = make_donor_pool_for_simulation_runner(),
        entrant_targets =
          make_entrant_targets_for_simulation_runner()
      ),
      "`settings` must be a `ponte_settings` object.",
      fixed = TRUE
    )
  }
)
