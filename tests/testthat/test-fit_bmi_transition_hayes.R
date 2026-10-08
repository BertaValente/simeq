test_that("simeq_fit_bmi_transition_hayes returns a fitted model object", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())

  expect_s3_class(fit, "simeq_bmi_transition_hayes")
  expect_s3_class(fit$model, "lm")
  expect_true(is.finite(fit$residual_rmse) && fit$residual_rmse > 0)
  expect_named(
    fit,
    c("model", "residual_rmse", "stratum_summary", "annual_changes",
      "regression_data", "stable_tolerance")
  )
})

test_that("annual changes are only built for 5, 7 or 9 year survey intervals", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())

  expect_true(all(fit$annual_changes$interval_years %in% c(5L, 7L, 9L)))
  expect_setequal(
    unique(paste(fit$annual_changes$year_t, fit$annual_changes$year_t1)),
    c("2005 2014", "2014 2019")
  )
})

test_that("annual change equals the difference in stratum means over the interval", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel())
  ch <- fit$annual_changes[1, ]

  expect_equal(
    ch$annual_bmi_change,
    (ch$mean_bmi_t1 - ch$mean_bmi_t) / ch$interval_years
  )
})

test_that("change_type respects stable_tolerance", {
  fit <- simeq_fit_bmi_transition_hayes(make_hayes_bmi_panel(), stable_tolerance = 100)

  expect_true(all(fit$annual_changes$change_type == "stable"))
})

test_that("simeq_fit_bmi_transition_hayes requires age_group", {
  panel <- make_hayes_bmi_panel()
  panel$age_group <- NULL

  expect_error(
    simeq_fit_bmi_transition_hayes(panel),
    "must contain `age_group`"
  )
})

test_that("simeq_fit_bmi_transition_hayes validates stable_tolerance", {
  panel <- make_hayes_bmi_panel()

  expect_error(simeq_fit_bmi_transition_hayes(panel, stable_tolerance = -1), "stable_tolerance")
  expect_error(simeq_fit_bmi_transition_hayes(panel, stable_tolerance = NA_real_), "stable_tolerance")
  expect_error(simeq_fit_bmi_transition_hayes(panel, stable_tolerance = c(0.1, 0.2)), "stable_tolerance")
})

test_that("simeq_fit_bmi_transition_hayes errors when no valid interval exists", {
  panel <- make_hayes_bmi_panel()
  panel <- panel[panel$year != 2014L, ]
  # 2005 -> 2019 is a 14-year gap, which is not an accepted interval.

  expect_error(
    simeq_fit_bmi_transition_hayes(panel),
    "No annual BMI changes could be constructed"
  )
})
