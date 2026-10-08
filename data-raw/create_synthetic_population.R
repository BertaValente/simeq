# data-raw/create_synthetic_population.R
#
# Generates a small, fully synthetic baseline population for use in the
# simeq vignette and package examples. This dataset contains no real
# individual data from the Portuguese National Health Survey; it is produced
# entirely from parametric distributions calibrated to approximate realistic
# Portuguese adult BMI and demographic patterns.
#
# Run this script to regenerate data/simeq_synthetic_pop.rda.

set.seed(6214)

n <- 2000L

# Age: skewed towards working-age adults, ranging 20-84
ages <- sample(
  20:84, n,
  replace = TRUE,
  prob = dnorm(20:84, mean = 45, sd = 18)
)

# Sex: approximately equal split (Portuguese adult sex ratio)
sexes <- sample(c("men", "women"), n, replace = TRUE, prob = c(0.49, 0.51))

# Education (2-level): older adults have higher probability of lower education
edu_prob_low <- ifelse(ages > 50, 0.65, 0.45)
education <- mapply(
  function(p) sample(c("low", "high"), 1, prob = c(p, 1 - p)),
  edu_prob_low
)

# BMI: sex/age-adjusted with realistic Portuguese mean values
bmi_mean <- ifelse(
  sexes == "men",
  25 + (ages - 20) * 0.06,
  24 + (ages - 20) * 0.07
)
bmis <- pmax(15, pmin(60, rnorm(n, mean = bmi_mean, sd = 4.5)))

# Height (m): sex-specific population mean with realistic SD
height_mean_cm <- ifelse(sexes == "men", 173, 161)
heights_m <- pmax(1.45, pmin(2.10, rnorm(n, mean = height_mean_cm, sd = 7))) / 100

# Weight (kg): back-calculated from BMI and height
weights_kg <- bmis * heights_m^2

# Age groups (5-year bands)
age_group <- cut(
  ages,
  breaks = seq(20, 85, by = 5),
  right = FALSE,
  include.lowest = TRUE
)

simeq_synthetic_pop <- tibble::tibble(
  person_id        = paste0("synth_", seq_len(n)),
  age              = as.integer(ages),
  sex              = sexes,
  education_level  = as.character(education),
  education_level2 = as.character(education),
  survey_weight    = 1.0,
  bmi              = round(bmis, 1),
  weight           = round(weights_kg, 1),
  height           = round(heights_m, 3),
  age_group        = age_group,
  alive            = TRUE
)

usethis::use_data(simeq_synthetic_pop, overwrite = TRUE, compress = "bzip2")
