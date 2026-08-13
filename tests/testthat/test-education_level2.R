test_that(
  "ponte_prepare_education_level2 maps three education levels",
  {
    state <- data.frame(
      education_level = c(
        "low",
        "medium",
        "high"
      )
    )

    updated_state <- ponte_prepare_education_level2(
      state = state,
      source = "map_3_to_2"
    )

    expect_identical(
      updated_state$education_level2,
      c(
        "lower",
        "lower",
        "higher"
      )
    )

    expect_identical(
      updated_state$education_level,
      c(
        "low",
        "medium",
        "high"
      )
    )
  }
)

test_that(
  "ponte_prepare_education_level2 accepts an existing valid variable",
  {
    state <- data.frame(
      education_level2 = c(
        "lower",
        "higher"
      )
    )

    updated_state <- ponte_prepare_education_level2(
      state = state,
      source = "existing"
    )

    expect_identical(
      updated_state$education_level2,
      c(
        "lower",
        "higher"
      )
    )
  }
)

test_that(
  "ponte_prepare_education_level2 requires the relevant input column",
  {
    state <- data.frame(
      education_level = "low"
    )

    expect_error(
      ponte_prepare_education_level2(
        state = state,
        source = "existing"
      ),
      "`state` must contain `education_level2`"
    )

    state <- data.frame(
      education_level2 = "lower"
    )

    expect_error(
      ponte_prepare_education_level2(
        state = state,
        source = "map_3_to_2"
      ),
      "`state` must contain `education_level`"
    )
  }
)

test_that(
  "ponte_prepare_education_level2 rejects invalid existing values",
  {
    state <- data.frame(
      education_level2 = c(
        "lower",
        "medium"
      )
    )

    expect_error(
      ponte_prepare_education_level2(
        state = state,
        source = "existing"
      ),
      "`education_level2` must contain only `lower` and `higher`"
    )
  }
)

test_that(
  "ponte_prepare_education_level2 rejects invalid three-level values",
  {
    state <- data.frame(
      education_level = c(
        "low",
        "university"
      )
    )

    expect_error(
      ponte_prepare_education_level2(
        state = state,
        source = "map_3_to_2"
      ),
      "`education_level` must contain only `low`, `medium`, and `high`"
    )
  }
)

test_that(
  "ponte_prepare_education_level2 requires a data frame",
  {
    expect_error(
      ponte_prepare_education_level2(
        state = c(
          "low",
          "medium"
        ),
        source = "map_3_to_2"
      ),
      "`state` must be a data frame"
    )
  }
)
