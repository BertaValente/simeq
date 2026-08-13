make_state_for_annual_update <- function() {
  population <- data.frame(
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

  ponte_initialise_population(
    population = population,
    settings = ponte_settings(
      initial_year = 2020L
    )
  )
}

make_life_table_for_annual_update <- function() {
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

make_bmi_risks_for_annual_update <- function() {
  rr_bmi <- expand.grid(
    sex = c(
      "men",
      "women"
    ),
    bmi_category = c(
      "underweight",
      "normal_weight",
      "overweight",
      "obesity_I",
      "obesity_II",
      "obesity_III"
    ),
    stringsAsFactors = FALSE
  )

  rr_bmi$rr_bmi <- 1

  rr_bmi
}

make_education_risks_for_annual_update <- function() {
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

make_donor_pool_for_annual_update <- function() {
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

make_entrant_targets_for_annual_update <- function() {
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
  "ponte_advance_one_year updates survivors and adds entrants",
  {
    set.seed(123)

    bmi_transition_model <- ponte_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    annual_result <- ponte_advance_one_year(
      state = make_state_for_annual_update(),
      life_table = make_life_table_for_annual_update(),
      rr_bmi = make_bmi_risks_for_annual_update(),
      rr_education = make_education_risks_for_annual_update(),
      bmi_transition_model = bmi_transition_model,
      donor_pool = make_donor_pool_for_annual_update(),
      entrant_targets = make_entrant_targets_for_annual_update(),
      n_entrants = 100L,
      education_source = "existing"
    )

    expect_identical(
      nrow(annual_result$living_state),
      102L
    )

    expect_identical(
      nrow(annual_result$deaths),
      0L
    )

    expect_true(
      all(
        annual_result$living_state$year == 2021L
      )
    )

    expect_identical(
      annual_result$living_state$age[
        annual_result$living_state$person_id == "person_1"
      ],
      31L
    )

    expect_identical(
      annual_result$living_state$age[
        annual_result$living_state$person_id == "person_2"
      ],
      84L
    )
  }
)

test_that(
  "ponte_advance_one_year creates age-20 entrants",
  {
    set.seed(123)

    bmi_transition_model <- ponte_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    annual_result <- ponte_advance_one_year(
      state = make_state_for_annual_update(),
      life_table = make_life_table_for_annual_update(),
      rr_bmi = make_bmi_risks_for_annual_update(),
      rr_education = make_education_risks_for_annual_update(),
      bmi_transition_model = bmi_transition_model,
      donor_pool = make_donor_pool_for_annual_update(),
      entrant_targets = make_entrant_targets_for_annual_update(),
      n_entrants = 100L,
      education_source = "existing"
    )

    entrants <- annual_result$living_state[
      grepl(
        "^entrant_",
        annual_result$living_state$person_id
      ),
      ,
      drop = FALSE
    ]

    expect_identical(
      nrow(entrants),
      100L
    )

    expect_true(
      all(
        entrants$age == 20L
      )
    )

    expect_true(
      all(
        entrants$year == 2021L
      )
    )

    expect_true(
      all(
        entrants$education_level2 %in%
          c(
            "lower",
            "higher"
          )
      )
    )
  }
)

test_that(
  "ponte_advance_one_year calibrates entrant weights by sex",
  {
    set.seed(123)

    bmi_transition_model <- ponte_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    annual_result <- ponte_advance_one_year(
      state = make_state_for_annual_update(),
      life_table = make_life_table_for_annual_update(),
      rr_bmi = make_bmi_risks_for_annual_update(),
      rr_education = make_education_risks_for_annual_update(),
      bmi_transition_model = bmi_transition_model,
      donor_pool = make_donor_pool_for_annual_update(),
      entrant_targets = make_entrant_targets_for_annual_update(),
      n_entrants = 100L,
      education_source = "existing"
    )

    entrants <- annual_result$living_state[
      grepl(
        "^entrant_",
        annual_result$living_state$person_id
      ),
      ,
      drop = FALSE
    ]

    entrant_weight_by_sex <- aggregate(
      survey_weight ~ sex,
      data = entrants,
      FUN = sum
    )

    expect_equal(
      entrant_weight_by_sex$survey_weight[
        entrant_weight_by_sex$sex == "men"
      ],
      49000
    )

    expect_equal(
      entrant_weight_by_sex$survey_weight[
        entrant_weight_by_sex$sex == "women"
      ],
      48000
    )
  }
)

test_that(
  "ponte_advance_one_year records deaths in the next simulation year",
  {
    set.seed(123)

    life_table <- make_life_table_for_annual_update()

    life_table$qx <- 1

    bmi_transition_model <- ponte_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    annual_result <- ponte_advance_one_year(
      state = make_state_for_annual_update(),
      life_table = life_table,
      rr_bmi = make_bmi_risks_for_annual_update(),
      rr_education = make_education_risks_for_annual_update(),
      bmi_transition_model = bmi_transition_model,
      donor_pool = make_donor_pool_for_annual_update(),
      entrant_targets = make_entrant_targets_for_annual_update(),
      n_entrants = 100L,
      education_source = "existing"
    )

    expect_identical(
      nrow(annual_result$deaths),
      2L
    )

    expect_identical(
      annual_result$deaths$year,
      c(
        2021L,
        2021L
      )
    )

    expect_identical(
      annual_result$deaths$death_year,
      c(
        2021L,
        2021L
      )
    )

    expect_identical(
      nrow(annual_result$living_state),
      100L
    )
  }
)

test_that(
  "ponte_advance_one_year requires one state year",
  {
    state <- make_state_for_annual_update()

    state$year[2L] <- 2021L

    bmi_transition_model <- ponte_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    expect_error(
      ponte_advance_one_year(
        state = state,
        life_table = make_life_table_for_annual_update(),
        rr_bmi = make_bmi_risks_for_annual_update(),
        rr_education = make_education_risks_for_annual_update(),
        bmi_transition_model = bmi_transition_model,
        donor_pool = make_donor_pool_for_annual_update(),
        entrant_targets = make_entrant_targets_for_annual_update(),
        n_entrants = 100L
      ),
      "`state` must contain records from exactly one calendar year.",
      fixed = TRUE
    )
  }
)
