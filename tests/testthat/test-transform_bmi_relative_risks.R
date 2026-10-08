test_that("transform_bmi_relative_risks rescales per-5-unit RRs to the obesity contrast", {
  rr <- make_gbd_rr_matrix(rr_per_5 = 1.5)
  out <- transform_bmi_relative_risks(rr, bmi_reference = 22, bmi_obesity = 32)

  expect_equal(unique(out$rr_obesity_vs_normal), 1.5^2)
  expect_equal(unique(out$rr_obesity_vs_normal_lower), 1.4^2)
  expect_equal(unique(out$rr_obesity_vs_normal_upper), 1.6^2)
  expect_false("rr_overweight_vs_normal" %in% names(out))
})

test_that("overweight columns are added when bmi_overweight is given", {
  out <- transform_bmi_relative_risks(
    make_gbd_rr_matrix(rr_per_5 = 1.5), bmi_reference = 22, bmi_overweight = 27, bmi_obesity = 32
  )

  expect_equal(unique(out$rr_overweight_vs_normal), 1.5)
})

test_that("output has one row per outcome and sex, averaged over age bands", {
  rr <- make_gbd_rr_matrix()
  rr$rr_per_5[rr$age_band == "20-24"] <- 2
  out <- transform_bmi_relative_risks(rr)

  expect_equal(nrow(out), length(unique(rr$disease)) * 2L)
  expect_equal(out$outcome_lc, tolower(out$outcome))
  expected <- mean(c(2^2, rep(1.3^2, 12)))
  expect_equal(out$rr_obesity_vs_normal[1], expected)
})

test_that("transform_bmi_relative_risks validates its arguments", {
  rr <- make_gbd_rr_matrix()

  expect_error(transform_bmi_relative_risks(rr[, -4]), "missing required column\\(s\\): rr_per_5")
  expect_error(transform_bmi_relative_risks(rr, bmi_reference = NA), "bmi_reference")
  expect_error(transform_bmi_relative_risks(rr, bmi_obesity = 20), "greater than bmi_reference")
  expect_error(transform_bmi_relative_risks(rr, bmi_overweight = 35), "between bmi_reference")
})
