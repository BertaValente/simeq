test_that(
  "simeq_bmi_category classifies BMI values correctly",
  {
    result <- simeq_bmi_category(
      c(
        18.4,
        18.5,
        24.9,
        25.0,
        29.9,
        30.0,
        34.9,
        35.0,
        39.9,
        40.0,
        NA_real_
      )
    )

    expect_identical(
      result,
      c(
        "underweight",
        "normal_weight",
        "normal_weight",
        "overweight",
        "overweight",
        "obesity_I",
        "obesity_I",
        "obesity_II",
        "obesity_II",
        "obesity_III",
        NA_character_
      )
    )
  }
)

test_that(
  "simeq_bmi_category rejects non-numeric input",
  {
    expect_error(
      simeq_bmi_category(
        c(
          "22.0",
          "30.0"
        )
      ),
      "`bmi` must be numeric"
    )
  }
)

test_that(
  "simeq_bmi_category rejects infinite values",
  {
    expect_error(
      simeq_bmi_category(
        c(
          22.0,
          Inf
        )
      ),
      "`bmi` must not contain infinite"
    )
  }
)

