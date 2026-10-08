test_that(
  "simeq_update_bmi updates BMI-derived variables",
  {
    set.seed(123)

    bmi_transition_model <- simeq_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    state <- make_living_state_for_bmi_prediction()

    updated_state <- simeq_update_bmi(
      state = state,
      bmi_transition_model = bmi_transition_model
    )

    expect_identical(
      nrow(updated_state),
      nrow(state)
    )

    expect_true(
      all(
        updated_state$bmi >= 10
      )
    )

    expect_true(
      all(
        updated_state$bmi <= 80
      )
    )

    expect_identical(
      updated_state$bmi_category,
      simeq_bmi_category(
        updated_state$bmi
      )
    )

    expect_identical(
      updated_state$obesity,
      updated_state$bmi >= 30
    )
  }
)

test_that(
  "simeq_update_bmi retains both education variables",
  {
    set.seed(123)

    bmi_transition_model <- simeq_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    state <- make_living_state_for_bmi_prediction()

    updated_state <- simeq_update_bmi(
      state = state,
      bmi_transition_model = bmi_transition_model
    )

    expect_identical(
      updated_state$education_level,
      state$education_level
    )

    expect_identical(
      updated_state$education_level2,
      state$education_level2
    )
  }
)

test_that(
  "simeq_update_bmi constrains BMI to requested limits",
  {
    set.seed(123)

    bmi_transition_model <- simeq_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    state <- make_living_state_for_bmi_prediction()

    updated_state <- simeq_update_bmi(
      state = state,
      bmi_transition_model = bmi_transition_model,
      bmi_limits = c(
        25,
        25.1
      )
    )

    expect_true(
      all(
        updated_state$bmi >= 25
      )
    )

    expect_true(
      all(
        updated_state$bmi <= 25.1
      )
    )
  }
)

test_that(
  "simeq_update_bmi validates BMI limits",
  {
    bmi_transition_model <- simeq_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    expect_error(
      simeq_update_bmi(
        state = make_living_state_for_bmi_prediction(),
        bmi_transition_model = bmi_transition_model,
        bmi_limits = c(
          80,
          10
        )
      ),
      "`bmi_limits` must contain two finite values in increasing order.",
      fixed = TRUE
    )
  }
)
