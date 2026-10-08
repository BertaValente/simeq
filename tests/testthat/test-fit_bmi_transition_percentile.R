make_percentile_panel <- function() {
  # Two waves five years apart. Between waves the median of every cohort
  # rises by 0.5 kg/m2 (0.1 a year) and the spread doubles around it, so the
  # upper percentiles rise faster than the median and the lower ones fall.
  set.seed(1)
  rows <- list()
  offsets <- seq(-4, 4, length.out = 40)

  for (sex in c("men", "women")) {
    for (education in c("lower", "higher")) {
      for (age in seq(20L, 79L)) {
        rows[[length(rows) + 1L]] <- data.frame(
          year = 2014L,
          sex = sex,
          age = age,
          education_level2 = education,
          survey_weight = 1,
          bmi = 25 + offsets
        )
        rows[[length(rows) + 1L]] <- data.frame(
          year = 2019L,
          sex = sex,
          age = age + 5L,
          education_level2 = education,
          survey_weight = 1,
          bmi = 25.5 + 2 * offsets
        )
      }
    }
  }

  panel <- do.call(rbind, rows)
  panel[panel$age <= 84L, , drop = FALSE]
}

test_that("percentile model captures faster change in the upper tail", {
  model <- simeq_fit_bmi_transition_percentile(make_percentile_panel())

  expect_s3_class(model, "simeq_bmi_percentile_model")
  expect_identical(model$periods, 2014L)
  expect_identical(model$projection_period, 2014L)
  expect_length(model$models, length(model$probs))

  change_by_prob <- tapply(
    model$cohorts$annual_bmi_change,
    model$cohorts$prob,
    mean
  )

  expect_equal(unname(change_by_prob["0.5"]), 0.1, tolerance = 0.02)
  expect_gt(change_by_prob[["0.9"]], change_by_prob[["0.5"]])
  expect_lt(change_by_prob[["0.1"]], change_by_prob[["0.5"]])
  expect_true(all(diff(change_by_prob) > 0))
})

test_that("cohorts follow the same birth cohort across the interval", {
  model <- simeq_fit_bmi_transition_percentile(make_percentile_panel())

  # Baseline 75-79 plus five years reaches 80-84, the last observable group.
  expect_identical(max(model$cohorts$age_group_start), 75L)
  expect_identical(min(model$cohorts$age_group_start), 20L)
})

test_that("update gives heavier people in a group a larger BMI change", {
  model <- simeq_fit_bmi_transition_percentile(make_percentile_panel())

  state <- data.frame(
    sex = "women",
    age = 40L,
    education_level2 = "lower",
    survey_weight = 1,
    bmi = seq(20, 32, length.out = 25)
  )

  updated <- simeq_update_bmi_percentile(state, model)
  change <- updated$bmi - state$bmi

  expect_true(all(diff(change) >= -1e-12))
  expect_gt(change[25], change[13])
  expect_lt(change[1], change[13])
  expect_equal(mean(change), 0.1, tolerance = 0.03)
  expect_identical(updated$obesity, updated$bmi >= 30)
  expect_identical(
    as.character(updated$bmi_category),
    as.character(simeq_bmi_category(updated$bmi))
  )
})

test_that("ranks are computed within sex, education and age group", {
  model <- simeq_fit_bmi_transition_percentile(make_percentile_panel())

  # Same BMI in two groups: the person on top of the lighter group gets the
  # top-percentile change even though their BMI is lower than everyone in the
  # heavier group.
  state <- data.frame(
    sex = c(rep("men", 10), rep("women", 10)),
    age = 40L,
    education_level2 = "lower",
    survey_weight = 1,
    bmi = c(seq(20, 24, length.out = 10), seq(30, 34, length.out = 10))
  )

  change <- simeq_update_bmi_percentile(state, model)$bmi - state$bmi

  expect_gt(change[10], change[11])
})

test_that("BMI limits are respected and weight follows BMI", {
  model <- simeq_fit_bmi_transition_percentile(make_percentile_panel())

  state <- data.frame(
    sex = "men",
    age = 30L,
    education_level2 = "higher",
    survey_weight = 1,
    bmi = c(10, 25, 79.99),
    height = 170,
    weight = c(10, 25, 79.99) * 1.7^2
  )

  updated <- simeq_update_bmi_percentile(
    state,
    model,
    bmi_limits = c(10, 80)
  )

  expect_true(all(updated$bmi >= 10 & updated$bmi <= 80))
  expect_equal(updated$weight, updated$bmi * 1.7^2)
})

test_that("invalid inputs are rejected", {
  panel <- make_percentile_panel()
  model <- simeq_fit_bmi_transition_percentile(panel)

  expect_error(
    simeq_fit_bmi_transition_percentile(panel[, -6]),
    "missing required columns"
  )
  expect_error(
    simeq_fit_bmi_transition_percentile(panel, probs = c(0.5, 0.2)),
    "strictly increasing"
  )
  expect_error(
    simeq_fit_bmi_transition_percentile(panel[panel$year == 2014L, ]),
    "at least two survey years"
  )
  expect_error(
    simeq_fit_bmi_transition_percentile(panel, min_n = 1000L),
    "No synthetic cohort"
  )
  expect_error(
    simeq_update_bmi_percentile(data.frame(bmi = 25), model),
    "missing required columns"
  )
  expect_error(
    simeq_update_bmi_percentile(
      data.frame(
        sex = "men", age = 30L, education_level2 = "lower",
        survey_weight = 1, bmi = 25
      ),
      model,
      period = 2005L
    ),
    "fitted periods"
  )
  expect_error(
    simeq_update_bmi_percentile(data.frame(), list()),
    "simeq_fit_bmi_transition_percentile"
  )
})

test_that("percentile period follows the simulation year", {
  model <- list(periods = c(1998L, 2005L, 2014L), projection_period = 2014L)

  expect_identical(
    .bmi_percentile_period_for_year(model, 1998L, "by_year"),
    1998L
  )
  expect_identical(
    .bmi_percentile_period_for_year(model, 2010L, "by_year"),
    2005L
  )
  expect_identical(
    .bmi_percentile_period_for_year(model, 2030L, "by_year"),
    2014L
  )
  expect_identical(
    .bmi_percentile_period_for_year(model, 2000L, "latest"),
    2014L
  )
})

make_percentile_mortality_inputs <- function() {
  list(
    life_table = expand.grid(
      age = 20:84,
      sex = c("men", "women"),
      stringsAsFactors = FALSE
    ) |>
      transform(qx = 0.05),
    rr_bmi = expand.grid(
      sex = c("men", "women"),
      bmi_category = c(
        "underweight", "normal_weight", "overweight",
        "obesity_I", "obesity_II", "obesity_III"
      ),
      stringsAsFactors = FALSE
    ) |>
      transform(rr_bmi = rep(c(1.5, 1, 1.1, 1.5, 2, 2.7), each = 2)),
    rr_education = data.frame(
      education_level2 = c("lower", "higher"),
      rr_education = c(1, 1)
    )
  )
}

test_that("survival weighting removes survivor selection from the upper tail", {
  panel <- make_percentile_panel()
  mortality <- make_percentile_mortality_inputs()

  raw <- simeq_fit_bmi_transition_percentile(panel)
  adjusted <- simeq_fit_bmi_transition_percentile(
    panel,
    life_table = mortality$life_table,
    rr_bmi = mortality$rr_bmi,
    rr_education = mortality$rr_education
  )

  expect_false(raw$survival_weighted)
  expect_true(adjusted$survival_weighted)

  top <- max(raw$probs)
  raw_top <- mean(raw$cohorts$annual_bmi_change[raw$cohorts$prob == top])
  adjusted_top <- mean(
    adjusted$cohorts$annual_bmi_change[adjusted$cohorts$prob == top]
  )

  # People with obesity are less likely to survive, so the survivor-weighted
  # baseline is lower in the upper tail and the fitted change is larger.
  expect_gt(adjusted_top, raw_top)
})

test_that("survival weighting needs all three mortality inputs", {
  mortality <- make_percentile_mortality_inputs()

  expect_error(
    simeq_fit_bmi_transition_percentile(
      make_percentile_panel(),
      life_table = mortality$life_table
    ),
    "Supply all of"
  )
})

test_that("sex interactions add sex-specific age terms", {
  model <- simeq_fit_bmi_transition_percentile(
    make_percentile_panel(),
    sex_interactions = TRUE
  )

  expect_true(model$sex_interactions)
  expect_true("sexwomen:age_t" %in% names(stats::coef(model$models[[1L]])))
})
