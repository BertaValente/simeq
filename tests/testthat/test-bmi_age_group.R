test_that(
  "ponte_bmi_age_group assigns expected five-year groups",
  {
    expect_identical(
      ponte_bmi_age_group(
        c(
          20L,
          24L,
          25L,
          29L,
          30L,
          79L,
          80L,
          84L
        )
      ),
      c(
        "20_24",
        "20_24",
        "25_29",
        "25_29",
        "30_34",
        "75_79",
        "80_84",
        "80_84"
      )
    )
  }
)

test_that(
  "ponte_bmi_age_group accepts numeric integer values",
  {
    expect_identical(
      ponte_bmi_age_group(
        c(
          20,
          45,
          84
        )
      ),
      c(
        "20_24",
        "45_49",
        "80_84"
      )
    )
  }
)

test_that(
  "ponte_bmi_age_group rejects non-integer ages",
  {
    expect_error(
      ponte_bmi_age_group(
        c(
          20.5,
          25
        )
      ),
      "`age` must contain only integer values from 20 to 84.",
      fixed = TRUE
    )
  }
)

test_that(
  "ponte_bmi_age_group rejects ages below 20",
  {
    expect_error(
      ponte_bmi_age_group(
        c(
          19L,
          20L
        )
      ),
      "`age` must contain only integer values from 20 to 84.",
      fixed = TRUE
    )
  }
)

test_that(
  "ponte_bmi_age_group rejects ages above 84",
  {
    expect_error(
      ponte_bmi_age_group(
        c(
          84L,
          85L
        )
      ),
      "`age` must contain only integer values from 20 to 84.",
      fixed = TRUE
    )
  }
)
