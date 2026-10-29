# data-raw/create_synthetic_panel.R
#
# Generates `simeq_synthetic_panel`, a fully synthetic set of four repeated
# cross-sectional survey waves (1998, 2005, 2014, 2019) with the same columns
# as the National Health Survey (NHS) panel used by SIMEQ-PT. It lets users
# run Modules I-III end to end without the survey microdata (see
# vignette("synthetic-example")).
#
# Every value is drawn from the simple parametric distributions written below.
# No NHS record, weight or summary table is read or copied. The parameters are
# rough, rounded values chosen so that the data behave plausibly (BMI rising
# with age and over time, more weight gain at the top of the distribution,
# higher education rising across birth cohorts and completed in the twenties);
# they are not estimates and results from these data are illustrative only.
#
# Run from the package root to regenerate data/simeq_synthetic_panel.rda.

set.seed(20261029)

waves <- c(1998L, 2005L, 2014L, 2019L)
n_per_wave <- 5000L
# Approximate adult population aged 20-84, used only to scale the weights
adult_population <- c(`1998` = 7.2e6, `2005` = 7.6e6, `2014` = 7.8e6,
                      `2019` = 7.9e6)

make_wave <- function(year) {
  n <- n_per_wave

  # Age: population ages over time
  age_prob <- stats::dnorm(20:84, mean = 44 + 0.15 * (year - 1998), sd = 19)
  age <- sample(20:84, n, replace = TRUE, prob = age_prob)
  sex <- sample(c("men", "women"), n, replace = TRUE, prob = c(0.48, 0.52))
  women <- sex == "women"
  birth_year <- year - age

  # Education: final level by birth cohort; younger cohorts and women born
  # after 1960 more often reach higher education, which is completed between
  # ages 20 and 29 (before that, future graduates report secondary education)
  cohort <- (birth_year - 1950) / 10
  p_low <- stats::plogis(1.8 - 0.75 * cohort)
  p_high_if_not_low <- stats::plogis(-0.6 + 0.25 * cohort +
                                       0.4 * (women & birth_year > 1960))
  final_level <- ifelse(
    stats::runif(n) < p_low, "low",
    ifelse(stats::runif(n) < p_high_if_not_low, "high", "medium")
  )
  share_completed <- pmin(1, pmax(0, (age - 19) / 10))
  education_level <- ifelse(
    final_level == "high" & stats::runif(n) > share_completed,
    "medium", final_level
  )
  education_level2 <- ifelse(education_level == "high", "higher", "lower")

  # BMI: right-skewed (log-normal) around a sex-, age-, education- and
  # period-specific mean; the spread widens over time so the upper
  # percentiles rise faster than the median
  a <- age - 20
  t <- year - 1998
  mean_bmi <- ifelse(
    women,
    22.4 + 0.20 * a - 0.0022 * a^2 + 0.035 * t - 1.6 * (education_level2 == "higher"),
    23.6 + 0.19 * a - 0.0024 * a^2 + 0.045 * t - 0.8 * (education_level2 == "higher")
  )
  sd_log <- ifelse(women, 0.155, 0.135) + 0.0012 * t
  bmi <- exp(stats::rnorm(n, log(mean_bmi) - sd_log^2 / 2, sd_log))
  bmi <- round(pmin(pmax(bmi, 15), 60), 1)

  # Height (cm) and weight (kg), consistent with BMI
  height <- ifelse(
    women,
    stats::rnorm(n, 158 + 0.08 * (birth_year - 1950), 6.5),
    stats::rnorm(n, 170 + 0.09 * (birth_year - 1950), 7)
  )
  height <- round(pmin(pmax(height, 140), 205), 1)
  weight <- round(bmi * (height / 100)^2, 1)

  # Survey weights: unequal, summing to the adult population
  raw_weight <- exp(stats::rnorm(n, 0, 0.35))
  survey_weight <- raw_weight / sum(raw_weight) *
    adult_population[[as.character(year)]]

  tibble::tibble(
    person_id = sprintf("synth%d_%04d", year, seq_len(n)),
    year = as.integer(year),
    sex = sex,
    age = as.integer(age),
    education_level = education_level,
    education_level2 = education_level2,
    survey_weight = survey_weight,
    bmi = bmi,
    height = height,
    weight = weight
  )
}

simeq_synthetic_panel <- dplyr::bind_rows(lapply(waves, make_wave))

usethis::use_data(simeq_synthetic_panel, overwrite = TRUE, compress = "xz")
