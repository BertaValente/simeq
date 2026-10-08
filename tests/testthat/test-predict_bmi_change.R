test_that(
  "simeq_predict_bmi_change returns one value per living record",
  {
    set.seed(123)

    bmi_transition_model <- simeq_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    state <- make_living_state_for_bmi_prediction()

    bmi_change <- simeq_predict_bmi_change(
      state = state,
      bmi_transition_model = bmi_transition_model
    )

    expect_type(
      bmi_change,
      "double"
    )

    expect_identical(
      length(bmi_change),
      nrow(state)
    )

    expect_true(
      all(
        is.finite(bmi_change)
      )
    )
  }
)

test_that(
  "simeq_predict_bmi_change respects transition limits",
  {
    set.seed(123)

    bmi_transition_model <- simeq_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit(),
      transition_limits = c(
        -0.01,
        0.01
      )
    )

    bmi_change <- simeq_predict_bmi_change(
      state = make_living_state_for_bmi_prediction(),
      bmi_transition_model = bmi_transition_model
    )

    expect_true(
      all(
        bmi_change >= -0.01
      )
    )

    expect_true(
      all(
        bmi_change <= 0.01
      )
    )
  }
)

test_that(
  "simeq_predict_bmi_change requires education_level2",
  {
    state <- make_living_state_for_bmi_prediction()

    state$education_level2 <- NULL

    bmi_transition_model <- simeq_fit_bmi_transition(
      bmi_transitions = make_bmi_transitions_for_model_fit()
    )

    expect_error(
      simeq_predict_bmi_change(
        state = state,
        bmi_transition_model = bmi_transition_model
      ),
      "`state` must contain `education_level2`.",
      fixed = TRUE
    )
  }
)

test_that(
  "simeq_predict_bmi_change rejects an invalid model object",
  {
    expect_error(
      simeq_predict_bmi_change(
        state = make_living_state_for_bmi_prediction(),
        bmi_transition_model = list()
      ),
      "`bmi_transition_model` must be a `simeq_bmi_transition_model` object.",
      fixed = TRUE
    )
  }
)
