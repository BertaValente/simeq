make_mortality_risks_for_validation <- function() {
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

  rr_bmi$rr_bmi <- c(
    1.8,
    1.8,
    1.0,
    1.0,
    1.2,
    1.2,
    1.5,
    1.5,
    2.0,
    2.0,
    3.0,
    3.0
  )
  rr_education <- data.frame(
    education_level2 = c(
      "lower",
      "higher"
    ),
    rr_education = c(
      1.0,
      0.9
    )
  )

  list(
    rr_bmi = rr_bmi,
    rr_education = rr_education
  )
}

test_that(
  "simeq_validate_mortality_risks accepts valid complete risk tables",
  {
    risks <- make_mortality_risks_for_validation()

    expect_invisible(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      )
    )
  }
)

test_that(
  "simeq_validate_mortality_risks requires BMI risk table columns",
  {
    risks <- make_mortality_risks_for_validation()

    risks$rr_bmi$rr_bmi <- NULL

    expect_error(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      ),
      "`rr_bmi` is missing required columns: rr_bmi"
    )
  }
)

test_that(
  "simeq_validate_mortality_risks requires education risk table columns",
  {
    risks <- make_mortality_risks_for_validation()

    risks$rr_education$rr_education <- NULL

    expect_error(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      ),
      "`rr_education` is missing required columns: rr_education"
    )
  }
)

test_that(
  "simeq_validate_mortality_risks requires positive finite BMI risks",
  {
    risks <- make_mortality_risks_for_validation()

    risks$rr_bmi$rr_bmi[1] <- 0

    expect_error(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      ),
      "`rr_bmi\\$rr_bmi` must contain positive, finite values"
    )
  }
)

test_that(
  "simeq_validate_mortality_risks requires positive finite education risks",
  {
    risks <- make_mortality_risks_for_validation()

    risks$rr_education$rr_education[1] <- -0.1

    expect_error(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      ),
      "`rr_education\\$rr_education` must contain positive, finite values"
    )
  }
)

test_that(
  "simeq_validate_mortality_risks requires all BMI categories",
  {
    risks <- make_mortality_risks_for_validation()

    risks$rr_bmi <- risks$rr_bmi[
      risks$rr_bmi$bmi_category != "obesity_II",
      ,
      drop = FALSE
    ]

    expect_error(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      ),
      "`rr_bmi` must cover every sex-BMI-category combination"
    )
  }
)

test_that(
  "simeq_validate_mortality_risks requires both education levels",
  {
    risks <- make_mortality_risks_for_validation()

    risks$rr_education <- risks$rr_education[
      risks$rr_education$education_level2 == "lower",
      ,
      drop = FALSE
    ]

    expect_error(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      ),
      "`rr_education` must cover both `lower` and `higher` education"
    )
  }
)

test_that(
  "simeq_validate_mortality_risks rejects duplicate BMI risk strata",
  {
    risks <- make_mortality_risks_for_validation()

    risks$rr_bmi <- rbind(
      risks$rr_bmi,
      risks$rr_bmi[1, ]
    )

    expect_error(
      simeq_validate_mortality_risks(
        rr_bmi = risks$rr_bmi,
        rr_education = risks$rr_education
      ),
      "`rr_bmi` must contain one record per sex-BMI-category combination"
    )
  }
)
