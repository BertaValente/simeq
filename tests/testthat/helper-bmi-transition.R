make_bmi_transitions_for_model_fit <- function() {
  model_data <- expand.grid(
    sex = c(
      "men",
      "women"
    ),
    education_level2 = c(
      "lower",
      "higher"
    ),
    age_t = c(
      20,
      25,
      30,
      35,
      40
    ),
    year_t = c(
      2010,
      2015,
      2020
    ),
    stringsAsFactors = FALSE
  )

  model_data$row_id <- seq_len(
    nrow(model_data)
  )

  model_data$bmi_t <- 20 +
    0.15 * model_data$age_t +
    ifelse(
      model_data$sex == "women",
      0.8,
      0
    ) +
    ifelse(
      model_data$education_level2 == "higher",
      -0.5,
      0
    ) +
    (model_data$row_id %% 4) * 0.3

  model_data$annual_bmi_change <- 0.06 +
    ifelse(
      model_data$sex == "women",
      0.02,
      0
    ) +
    0.008 * model_data$age_t -
    0.0001 * model_data$age_t ^ 2 +
    0.012 * model_data$bmi_t +
    ifelse(
      model_data$education_level2 == "higher",
      -0.015,
      0
    ) +
    ifelse(
      model_data$year_t == 2015,
      0.01,
      0
    ) +
    ifelse(
      model_data$year_t == 2020,
      0.02,
      0
    ) +
    (model_data$row_id %% 7) * 0.003

  # transition_weight: required by simeq_fit_bmi_transition().
  # Use geometric-mean-style weights (all equal here for simplicity).
  model_data$transition_weight <- 1000

  model_data
}

make_living_state_for_bmi_prediction <- function() {
  population <- data.frame(
    person_id = c(
      "person_1",
      "person_2",
      "person_3",
      "person_4"
    ),
    age = c(
      25L,
      30L,
      35L,
      40L
    ),
    sex = c(
      "men",
      "women",
      "men",
      "women"
    ),
    education_level = c(
      "low",
      "medium",
      "high",
      "high"
    ),
    education_level2 = c(
      "lower",
      "lower",
      "higher",
      "higher"
    ),
    bmi = c(
      22,
      25,
      29,
      31
    ),
    survey_weight = c(
      100,
      100,
      100,
      100
    ),
    stringsAsFactors = FALSE
  )

  simeq_initialise_population(
    population = population,
    settings = simeq_settings()
  )
}
