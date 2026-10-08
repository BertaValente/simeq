# Shared fixtures for the proposed tests of previously untested functions.

make_hayes_bmi_panel <- function(n = 3000L, seed = 1L) {
  set.seed(seed)
  panel <- data.frame(
    year = sample(c(2005L, 2014L, 2019L), n, replace = TRUE),
    sex = sample(c("men", "women"), n, replace = TRUE),
    age = sample(20:84, n, replace = TRUE),
    education_level2 = sample(c("low_medium", "higher"), n, replace = TRUE),
    survey_weight = stats::runif(n, 500, 1500),
    stringsAsFactors = FALSE
  )
  panel$bmi <- 22 +
    0.08 * (panel$age - 20) +
    0.10 * (panel$year - 2005) +
    stats::rnorm(n, 0, 3)
  simeq_prepare_bmi_panel(panel)
}

make_hayes_population <- function() {
  data.frame(
    sex = c("men", "women"),
    age = c(40L, 60L),
    education_level2 = c("higher", "low_medium"),
    bmi = c(27, 32),
    stringsAsFactors = FALSE
  )
}

make_gbd_rr_matrix <- function(rr_per_5 = 1.3) {
  rr <- expand.grid(
    disease = obesity_cost_outcome_mapping()$outcome_pattern,
    sex = c("Males", "Females"),
    age_band = paste0(seq(20, 80, 5), "-", seq(24, 84, 5)),
    stringsAsFactors = FALSE
  )
  rr$rr_per_5 <- rr_per_5
  rr$rr_low <- rr_per_5 - 0.1
  rr$rr_high <- rr_per_5 + 0.1
  tibble::as_tibble(rr)
}

make_cost_simulation_output <- function(years = 2025:2027, n = 400L, seed = 2L) {
  set.seed(seed)
  sim <- expand.grid(year = years, id = seq_len(n))
  sim$age <- sample(20:84, nrow(sim), replace = TRUE)
  sim$sex <- sample(c("men", "women"), nrow(sim), replace = TRUE)
  sim$bmi <- stats::rnorm(nrow(sim), 27, 4)
  sim$alive <- TRUE
  sim$survey_weight <- 100
  sim
}
