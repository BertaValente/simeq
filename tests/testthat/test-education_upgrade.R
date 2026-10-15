make_education_panel <- function() {
  baseline <- data.frame(
    year = 2005L,
    sex = rep(c("men", "women"), each = 10L),
    age = 22L,
    education_level = rep(c(rep("medium", 6L), rep("high", 2L), rep("low", 2L)), 2L),
    survey_weight = 1
  )
  follow_up <- data.frame(
    year = 2010L,
    sex = rep(c("men", "women"), each = 10L),
    age = 27L,
    education_level = rep(c(rep("medium", 3L), rep("high", 5L), rep("low", 2L)), 2L),
    survey_weight = 1
  )
  rbind(baseline, follow_up)
}

test_that("simeq_fit_education_upgrade annualises cohort gains in high education", {
  rates <- simeq_fit_education_upgrade(make_education_panel(), age_groups = 20L)

  expect_s3_class(rates, "simeq_education_upgrade")
  expect_equal(nrow(rates), 2L)
  # High share rises from 0.2 to 0.5 over 5 years; medium share is 0.6.
  expect_equal(rates$annual_rate, rep(0.3 / 5 / 0.6, 2L))
  expect_equal(nrow(attr(rates, "cohorts")), 2L)
})

test_that("simeq_fit_education_upgrade sets falling cohort shares to zero", {
  panel <- make_education_panel()
  panel$education_level[panel$year == 2010L] <- "low"

  rates <- simeq_fit_education_upgrade(panel, age_groups = 20L)

  expect_equal(rates$annual_rate, c(0, 0))
})

test_that("simeq_fit_education_upgrade checks periods and columns", {
  expect_error(
    simeq_fit_education_upgrade(make_education_panel(), periods = 2010L),
    "periods"
  )
  expect_error(
    simeq_fit_education_upgrade(make_education_panel()[, -4L]),
    "education_level"
  )
})

test_that("simeq_update_education only upgrades medium education in the age range", {
  state <- data.frame(
    sex = c("men", "men", "men", "women", "women"),
    age = c(22L, 22L, 40L, 31L, 31L),
    education_level = c("medium", "low", "medium", "medium", "high"),
    education_level2 = c("lower", "lower", "lower", "lower", "higher"),
    stringsAsFactors = FALSE
  )
  rates <- data.frame(
    sex = c("men", "women"),
    age_min = c(20L, 30L),
    age_max = c(24L, 34L),
    annual_rate = c(1, 1)
  )

  updated <- simeq_update_education(state, rates)

  expect_equal(updated$education_level, c("high", "low", "medium", "high", "high"))
  expect_equal(
    updated$education_level2,
    c("higher", "lower", "lower", "higher", "higher")
  )

  rates$annual_rate <- 0
  expect_equal(simeq_update_education(state, rates), state)
})

test_that("simeq_combine_education_upgrade uses the rates of the state's year", {
  early <- data.frame(sex = c("men", "women"), age_min = 20L, age_max = 24L,
                      annual_rate = c(0, 0))
  late <- transform(early, annual_rate = c(1, 1))
  rates <- simeq_combine_education_upgrade(list(
    list(from = 1998L, to = 2004L, upgrade = early),
    list(from = 2005L, to = NA, upgrade = late)
  ))

  expect_s3_class(rates, "simeq_education_upgrade")
  expect_equal(nrow(rates), 4L)

  state <- data.frame(
    year = 2004L,
    sex = c("men", "women"),
    age = 22L,
    education_level = "medium",
    stringsAsFactors = FALSE
  )
  expect_equal(simeq_update_education(state, rates)$education_level,
               c("medium", "medium"))

  state$year <- 2005L
  expect_equal(simeq_update_education(state, rates)$education_level,
               c("high", "high"))
})

test_that("simeq_combine_education_upgrade rejects overlapping periods", {
  rates <- data.frame(sex = "men", age_min = 20L, age_max = 24L,
                      annual_rate = 0.1)
  expect_error(
    simeq_combine_education_upgrade(list(
      list(from = 1998L, to = 2005L, upgrade = rates),
      list(from = 2005L, to = NA, upgrade = rates)
    )),
    "overlap"
  )
})
