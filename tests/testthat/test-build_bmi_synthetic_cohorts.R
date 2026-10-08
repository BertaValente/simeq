
# Fixture: two survey waves (2015, 2020) each with two age groups (20, 25) and
# two sex × education strata. The five-year interval means each age cohort ages
# forward by five years exactly: 20→25 and 25→30. This produces four transitions.
make_bmi_panel_for_synthetic_cohorts <- function() {
  data.frame(
    year = c(
      2015L, 2015L, 2015L, 2015L,
      2020L, 2020L, 2020L, 2020L
    ),
    sex = c(
      "men", "men", "women", "women",
      "men", "men", "women", "women"
    ),
    age = c(
      20L, 25L, 20L, 25L,
      25L, 30L, 25L, 30L
    ),
    education_level2 = c(
      "lower", "lower", "higher", "higher",
      "lower", "lower", "higher", "higher"
    ),
    survey_weight = rep(100, 8),
    bmi = c(
      22, 25, 23, 26,   # 2015: age 20 and 25
      23, 26, 24, 27    # 2020: age 25 and 30
    ),
    stringsAsFactors = FALSE
  )
}

test_that(
  "simeq_build_bmi_synthetic_cohorts produces one row per cohort transition",
  {
    bmi_panel <- simeq_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_synthetic_cohorts()
    )

    transitions <- simeq_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    # Two age cohorts (20→25, 25→30) × two strata (men/lower, women/higher)
    expect_identical(nrow(transitions), 4L)

    expect_true(all(transitions$year_t  == 2015L))
    expect_true(all(transitions$year_t1 == 2020L))
    expect_true(all(transitions$interval_years == 5L))
  }
)

test_that(
  "simeq_build_bmi_synthetic_cohorts returns age_group_t and age_group_t1 columns",
  {
    bmi_panel <- simeq_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_synthetic_cohorts()
    )

    transitions <- simeq_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    # The function tracks age groups at both ends of the transition
    expect_true("age_group_t"  %in% names(transitions))
    expect_true("age_group_t1" %in% names(transitions))

    # No legacy single `age_group` column
    expect_false("age_group" %in% names(transitions))

    # Baseline age groups are the two starting cohorts
    expect_setequal(
      unique(as.character(transitions$age_group_t)),
      c("20_24", "25_29")
    )

    # Follow-up age groups are the corresponding cohorts five years on
    expect_setequal(
      unique(as.character(transitions$age_group_t1)),
      c("25_29", "30_34")
    )
  }
)

test_that(
  "simeq_build_bmi_synthetic_cohorts calculates annual BMI change correctly",
  {
    bmi_panel <- simeq_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_synthetic_cohorts()
    )

    transitions <- simeq_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    # Each BMI difference is 1 unit over a 5-year interval → 0.2 per year
    expect_equal(
      transitions$annual_bmi_change,
      rep(0.2, 4L)
    )

    # Verify the formula: (bmi_t1 - bmi_t) / interval_years
    expect_equal(
      transitions$annual_bmi_change,
      (transitions$bmi_t1 - transitions$bmi_t) / transitions$interval_years
    )
  }
)

test_that(
  "simeq_build_bmi_synthetic_cohorts creates a transition_weight column",
  {
    bmi_panel <- simeq_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_synthetic_cohorts()
    )

    transitions <- simeq_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    expect_true("transition_weight" %in% names(transitions))
    expect_true(all(is.finite(transitions$transition_weight)))
    expect_true(all(transitions$transition_weight > 0))

    # Geometric mean of equal weights (100, 100): sqrt(100 * 100) = 100
    expect_equal(transitions$transition_weight, rep(100, 4L))
  }
)

test_that(
  "simeq_build_bmi_synthetic_cohorts requires prepared data",
  {
    expect_error(
      simeq_build_bmi_synthetic_cohorts(
        bmi_panel = make_bmi_panel_for_synthetic_cohorts()
      ),
      "`bmi_panel` must contain an `age_group` column. ",
      fixed = TRUE
    )
  }
)

test_that(
  "simeq_build_bmi_synthetic_cohorts works with non-five-year survey intervals",
  {
    # A six-year gap (2015 → 2021) is not a multiple of the five-year age group.
    # The function should still produce valid transitions by snapping to the
    # nearest five-year group start for the follow-up cohort.
    bmi_panel <- make_bmi_panel_for_synthetic_cohorts()
    bmi_panel$year[bmi_panel$year == 2020L] <- 2021L

    bmi_panel <- simeq_prepare_bmi_panel(bmi_panel = bmi_panel)

    transitions <- simeq_build_bmi_synthetic_cohorts(
      bmi_panel = bmi_panel
    )

    expect_true(nrow(transitions) > 0L)
    expect_true(all(transitions$interval_years == 6L))

    # annual_bmi_change must use the actual interval (6), not 5
    expect_equal(
      transitions$annual_bmi_change,
      (transitions$bmi_t1 - transitions$bmi_t) / transitions$interval_years
    )
  }
)
