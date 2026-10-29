test_that("synthetic panel has the structure of the survey panel", {
  panel <- simeq_synthetic_panel

  expect_setequal(unique(panel$year), c(1998L, 2005L, 2014L, 2019L))
  expect_true(all(panel$age >= 20L & panel$age <= 84L))
  expect_setequal(unique(panel$sex), c("men", "women"))
  expect_setequal(unique(panel$education_level), c("low", "medium", "high"))
  expect_true(all(panel$survey_weight > 0))
  expect_true(all(panel$bmi >= 15 & panel$bmi <= 60))
  expect_equal(
    panel$education_level2,
    ifelse(panel$education_level == "high", "higher", "lower")
  )
})

test_that("Module I runs end to end on the synthetic panel", {
  panel <- simeq_synthetic_panel
  life_table <- tidyr::crossing(age = 20:84, sex = c("men", "women")) |>
    dplyr::mutate(qx = 0.0005 * exp(0.085 * (age - 20)))
  rr_bmi <- tidyr::crossing(
    sex = c("men", "women"),
    tibble::tibble(
      bmi_category = c("underweight", "normal_weight", "overweight",
                       "obesity_I", "obesity_II", "obesity_III"),
      rr_bmi = c(1.47, 1, 1.11, 1.44, 1.92, 2.71)
    )
  )
  rr_education <- tibble::tibble(
    education_level2 = c("lower", "higher"),
    rr_education = c(1, 0.9)
  )

  model <- simeq_fit_bmi_transition_percentile(
    panel,
    sex_interactions = TRUE,
    life_table = life_table,
    rr_bmi = rr_bmi,
    rr_education = rr_education
  )

  pool <- dplyr::filter(panel, year == 1998L, age <= 24L) |>
    dplyr::select(sex, age, education_level, bmi, survey_weight, weight, height)
  population <- dplyr::filter(panel, year == 1998L) |>
    dplyr::select(person_id, age, sex, education_level, education_level2,
                  bmi, survey_weight, weight, height)
  targets <- tidyr::crossing(entry_year = 1999:2001, sex = c("men", "women")) |>
    dplyr::mutate(entry_year = as.integer(entry_year),
                  entrant_population = 50000)

  sim <- simeq_run_simulation(
    population = population,
    settings = simeq_settings(initial_year = 1998L, projection_end_year = 2001L,
                              n_simulations = 1L, entrant_n_per_year = 50L,
                              seed = 1L),
    life_table = life_table,
    rr_bmi = rr_bmi,
    rr_education = rr_education,
    donor_pool = pool,
    entrant_targets = targets,
    bmi_update_method = "percentile",
    bmi_percentile_model = model,
    education_upgrade = simeq_fit_education_upgrade(panel)
  )

  expect_setequal(unique(sim$year), 1998:2001)
  expect_true(all(is.finite(sim$bmi[sim$alive])))
})
