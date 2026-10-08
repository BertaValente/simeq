make_donor_population <- function() {
  data.frame(
    sex = c("men", "men", "women", "women", "women", "men"),
    age = c(30L, 30L, 45L, 45L, 70L, 90L),
    education_level2 = c("higher", "low_medium", "higher", "low_medium", "low_medium", "higher"),
    bmi = c(24, 28, 22, 31, 29, 26),
    survey_weight = c(1000, 3000, 2000, 2000, 1000, 5000),
    stringsAsFactors = FALSE
  )
}

test_that("simeq_generate_synthetic_population returns n_agents with the expected columns", {
  out <- simeq_generate_synthetic_population(make_donor_population(), n_agents = 200L, seed = 1)

  expect_equal(nrow(out), 200L)
  expect_named(
    out,
    c("person_id", "age", "sex", "education_level", "education_level2",
      "survey_weight", "bmi", "age_group", "alive")
  )
  expect_false(anyDuplicated(out$person_id) > 0L)
  expect_true(all(out$alive))
})

test_that("weights sum to the represented population of eligible donors", {
  out <- simeq_generate_synthetic_population(make_donor_population(), n_agents = 50L, seed = 1)

  # The age-90 donor (weight 5000) is outside 20-84 and must be dropped.
  expect_equal(sum(out$survey_weight), 9000)
  expect_true(all(out$age >= 20L & out$age <= 84L))
})

test_that("sampled strata come from the donor sex-age-education combinations", {
  donors <- make_donor_population()
  out <- simeq_generate_synthetic_population(donors, n_agents = 300L, seed = 3)

  expect_true(all(
    paste(out$sex, out$age, out$education_level2) %in%
      paste(donors$sex, donors$age, donors$education_level2)
  ))
  expect_equal(out$education_level, out$education_level2)
})

test_that("a seed makes generation reproducible", {
  a <- simeq_generate_synthetic_population(make_donor_population(), n_agents = 30L, seed = 42)
  b <- simeq_generate_synthetic_population(make_donor_population(), n_agents = 30L, seed = 42)

  expect_identical(a, b)
})

test_that("bmi_jitter_sd = 0 reproduces donor BMI values exactly", {
  donors <- make_donor_population()
  out <- simeq_generate_synthetic_population(donors, n_agents = 100L, seed = 5, bmi_jitter_sd = 0)

  expect_true(all(out$bmi %in% donors$bmi))
})

test_that("simeq_generate_synthetic_population validates its inputs", {
  donors <- make_donor_population()

  expect_error(
    simeq_generate_synthetic_population(donors[, -5]),
    "Missing required columns: survey_weight"
  )

  donors$age <- 90L
  expect_error(
    simeq_generate_synthetic_population(donors, n_agents = 10L),
    "No eligible baseline donor records"
  )
})
