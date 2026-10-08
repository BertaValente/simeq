test_that("simeq_update_bmi_hayes applies the model prediction when not stochastic", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  pop <- make_hayes_population()

  out <- simeq_update_bmi_hayes(pop, fit, stochastic = FALSE)
  expected <- as.numeric(stats::predict(fit$model, newdata = pop))

  expect_equal(out$expected_annual_bmi_change, expected)
  expect_equal(out$random_annual_bmi_change, c(0, 0))
  expect_equal(out$bmi, pop$bmi + expected)
})

test_that("simeq_update_bmi_hayes is reproducible with a seed and clamps BMI", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  pop <- make_hayes_population()

  set.seed(10); a <- simeq_update_bmi_hayes(pop, fit)
  set.seed(10); b <- simeq_update_bmi_hayes(pop, fit)
  expect_identical(a$bmi, b$bmi)

  clamped <- simeq_update_bmi_hayes(pop, fit, stochastic = FALSE, min_bmi = 30, max_bmi = 30)
  expect_equal(clamped$bmi, c(30, 30))
})

test_that("simeq_update_bmi_hayes validates its inputs", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  pop <- make_hayes_population()

  expect_error(simeq_update_bmi_hayes(pop[, -4], fit), "missing required columns: bmi")
  expect_error(simeq_update_bmi_hayes(pop, list(residual_rmse = 1)), "fitted model")
  expect_error(simeq_update_bmi_hayes(pop, list(model = fit$model)), "residual_rmse")
})
