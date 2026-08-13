make_bmi_panel_for_synthetic_cohorts <- function() {
  data.frame(
    year = c(
      2015L,
      2015L,
      2015L,
      2015L,
      2020L,
      2020L,
      2020L,
      2020L
    ),
    sex = c(
      "men",
      "men",
      "women",
      "women",
      "men",
      "men",
      "women",
      "women"
    ),
    age = c(
      20L,
      25L,
      20L,
      25L,
      25L,
      30L,
      25L,
      30L
    ),
    education_level2 = c(
      "low_medium",
      "low_medium",
      "higher",
      "higher",
      "low_medium",
      "low_medium",
      "higher",
      "higher"
    ),
    survey_weight = c(
      100,
      100,
      100,
      100,
      100,
      100,
      100,
      100
    ),
    bmi = c(
      22,
      25,
      23,
      26,
      23,
      26,
      24,
      27
    ),
    stringsAsFactors = FALSE
  )
}

test_that(
  "ponte_build_bmi_synthetic_cohorts matches the same age group",
  {
    bmi_panel <- ponte_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_synthetic_cohorts()
    )

    transitions <- ponte_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    expect_identical(
      nrow(transitions),
      2L
    )

    expect_identical(
      transitions$year_t,
      c(
        2015L,
        2015L
      )
    )

    expect_identical(
      transitions$year_t1,
      c(
        2020L,
        2020L
      )
    )

    expect_identical(
      transitions$age_group,
      c(
        "25_29",
        "25_29"
      )
    )

    expect_identical(
      transitions$age_t,
      c(
        25L,
        25L
      )
    )

    expect_identical(
      transitions$age_t1,
      c(
        25L,
        25L
      )
    )
  }
)

test_that(
  "ponte_build_bmi_synthetic_cohorts calculates annual BMI change",
  {
    bmi_panel <- ponte_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_synthetic_cohorts()
    )

    transitions <- ponte_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    expect_equal(
      transitions$annual_bmi_change,
      c(
        -0.4,
        -0.4
      )
    )
  }
)

test_that(
  "ponte_build_bmi_synthetic_cohorts creates combined weights",
  {
    bmi_panel <- ponte_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_synthetic_cohorts()
    )

    transitions <- ponte_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    expect_equal(
      transitions$wt_combined,
      c(
        10000,
        10000
      )
    )
  }
)

test_that(
  "ponte_build_bmi_synthetic_cohorts requires prepared data",
  {
    expect_error(
      ponte_build_bmi_synthetic_cohorts(
        bmi_panel = make_bmi_panel_for_synthetic_cohorts()
      ),
      "`bmi_panel` must contain an `age_group` column. ",
      fixed = TRUE
    )
  }
)

test_that(
  "ponte_build_bmi_synthetic_cohorts requires a five-year survey pair",
  {
    bmi_panel <- make_bmi_panel_for_synthetic_cohorts()

    bmi_panel$year[
      bmi_panel$year == 2020L
    ] <- 2021L

    bmi_panel <- ponte_prepare_bmi_panel(
      bmi_panel = bmi_panel
    )

    expect_error(
      ponte_build_bmi_synthetic_cohorts(
        bmi_panel = bmi_panel
      ),
      "must contain at least one pair of survey years separated by 5 years",
      fixed = TRUE
    )
  }
)
