make_scenario_output <- function() {
  data.frame(
    year = c(2026L, 2026L, 2027L, 2027L, 2027L),
    alive = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    bmi = c(24, 31, 26, 27, 35),
    survey_weight = c(1, 1, 1, 1, 1)
  )
}

test_that("calculate_scenario_costs scales costs by high-BMI prevalence", {
  out <- calculate_scenario_costs(
    make_scenario_output(),
    base_cost_2018_keur = 100,
    base_high_bmi_prevalence_2018 = 0.5,
    cpi_2026 = 100,
    discount_rate = 0.03,
    base_year = 2026L
  )

  expect_equal(out$by_year$high_bmi_prevalence, c(0.5, 1))
  expect_equal(out$by_year$cost_2026_eur_undiscounted, c(100000, 200000))
  expect_equal(out$by_year$discount_factor, c(1, 1 / 1.03))
  expect_equal(
    out$overall$cumulative_cost_2026_eur_pv,
    100000 + 200000 / 1.03
  )
})

test_that("calculate_scenario_costs ignores dead individuals", {
  out <- calculate_scenario_costs(make_scenario_output(), 100, 0.5)

  # The dead person in 2027 has BMI 35 but must not affect prevalence.
  expect_equal(out$by_year$high_bmi_prevalence[2], 1)
})

test_that("calculate_scenario_costs checks required columns", {
  expect_error(
    calculate_scenario_costs(make_scenario_output()[, -3], 100, 0.5),
    "missing required columns: bmi"
  )
})
