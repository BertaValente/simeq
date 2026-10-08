test_that("simeq_update_bmi_historical picks the transition period containing the year", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  pop <- make_hayes_population()

  expect_equal(
    unique(simeq_update_bmi_historical(pop, 2010L, fit, stochastic = FALSE)$bmi_transition_period),
    "2005-2014"
  )
  expect_equal(
    unique(simeq_update_bmi_historical(pop, 2016L, fit, stochastic = FALSE)$bmi_transition_period),
    "2014-2019"
  )
})

test_that("empirical changes are used only for cells with enough observations", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  pop <- make_hayes_population()

  strict <- simeq_update_bmi_historical(pop, 2010L, fit, min_cell_n = 1e6L, stochastic = FALSE)
  expect_true(all(strict$bmi_transition_source == "regression_fallback"))
  expect_equal(
    strict$expected_annual_bmi_change,
    as.numeric(stats::predict(fit$model, newdata = pop))
  )

  lenient <- simeq_update_bmi_historical(pop, 2010L, fit, min_cell_n = 1L, stochastic = FALSE)
  expect_true(all(lenient$bmi_transition_source == "empirical"))
})

test_that("simeq_update_bmi_historical recomputes bmi_category and obesity", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  out <- simeq_update_bmi_historical(make_hayes_population(), 2010L, fit, stochastic = FALSE)

  expect_equal(out$bmi_category, simeq_bmi_category(out$bmi))
  expect_equal(out$obesity, out$bmi >= 30)
  expect_equal(out$bmi_category_before_update, c("overweight", "obesity_I"))
})

test_that("years after the last survey wave use regression predictions only", {
  # The source comment says years beyond the last NHS wave are extrapolated
  # "with Hayes regression predictions only", but `use_regression_only` is
  # set and never read, so empirical cell changes are still applied.
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  out <- simeq_update_bmi_historical(
    make_hayes_population(), 2030L, fit, min_cell_n = 1L, stochastic = FALSE
  )

  expect_true(all(out$bmi_transition_source == "regression_fallback"))
})

test_that("simeq_update_bmi_historical errors before the first survey wave", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())

  expect_error(
    simeq_update_bmi_historical(make_hayes_population(), 2000L, fit),
    "No unique historical BMI transition period"
  )
})

test_that("simeq_update_bmi_historical validates its inputs", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  pop <- make_hayes_population()

  expect_error(simeq_update_bmi_historical(pop[, -3], 2010L, fit), "education_level2")
  expect_error(
    simeq_update_bmi_historical(pop, 2010L, fit[c("model", "residual_rmse")]),
    "missing: annual_changes"
  )
})
