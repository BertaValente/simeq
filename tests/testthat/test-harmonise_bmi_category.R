test_that(
  "ponte_harmonise_bmi_category returns package-standard BMI labels",

  {
    input <- c(
      "underweight",
      "normalweight",
      "normal weight",
      "over_weight",
      "obesityclassI",
      "obesity II",
      "obesity_III"
    )

    expected <- c(
      "underweight",
      "normal_weight",
      "normal_weight",
      "overweight",
      "obesity_I",
      "obesity_II",
      "obesity_III"
    )

    expect_equal(
      ponte_harmonise_bmi_category(input),
      expected
    )
  }
)

test_that(
  "ponte_harmonise_bmi_category returns NA for unsupported labels",
  {
    input <- c("obesity", "unknown", NA_character_, "")

    expect_equal(
      ponte_harmonise_bmi_category(input),
      c(NA_character_, NA_character_, NA_character_, NA_character_)
    )
  }
)

test_that(
  "ponte_harmonise_bmi_category preserves canonical labels",
  {
    canonical <- c(
      "underweight",
      "normal_weight",
      "overweight",
      "obesity_I",
      "obesity_II",
      "obesity_III"
    )

    expect_equal(
      ponte_harmonise_bmi_category(canonical),
      canonical
    )
  }
)
