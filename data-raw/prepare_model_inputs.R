# prepare_model_inputs.R
# simeq model input preparation
#
# Run twice from the package root with devtools::load_all() available:
#   apply_reporting_bias_calibration <- TRUE   -> base case (calibrated BMI)
#     data-raw/model_inputs_prepared_reporting_bias_calibrated.rds
#   apply_reporting_bias_calibration <- FALSE  -> sensitivity (self-reported BMI)
#     data-raw/model_inputs_prepared_self_reported.rds
#
# The analysis reads year-specific life tables from
# data/life_tables/pt_life_table_model_input_1998_2024.csv; the single life
# table stored here is kept only so the inputs file stays complete.
#
# Baseline population: Portuguese NHS 1998
# BMI panel: NHS 1998, 2005, 2014, 2019
# Mortality: Portuguese life table
# Entrant donor pools: NHS respondents aged 20–24, by survey wave
# NHS cost fields are intentionally not used.

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(tibble)

devtools::load_all(".")

set.seed(123L)


# TRUE = base case (calibrated BMI); FALSE = self-reported BMI sensitivity.
apply_reporting_bias_calibration <- TRUE

scenario_name <- if (apply_reporting_bias_calibration) {
  "reporting_bias_calibrated"
} else {
  "self_reported"
}

here::i_am("data-raw/prepare_model_inputs.R")

project_root <- here::here()

nhs_file <- here::here(
  "data-raw",
  "national_health_surveys",
  "subset_final_with_use_costs_2019.csv"
)

life_table_file <- here::here(
  "data-raw",
  "mortality",
  "prob_dying_qx.xlsx"
)

output_file <- here::here(
  "data-raw",
  # Revision-1 calibration equations (age + region): new file name, so the
  # inputs of simulation v9 (equations with education) stay unchanged.
  paste0("model_inputs_prepared_", scenario_name,
         if (apply_reporting_bias_calibration) "_rev1" else "", ".rds")
)

if (!file.exists(nhs_file)) {
  stop("NHS input file not found: ", nhs_file, call. = FALSE)
}

if (!file.exists(life_table_file)) {
  stop("Life-table input file not found: ", life_table_file, call. = FALSE)
}

# -----------------------------------------------------------------------------
# Load raw data
# -----------------------------------------------------------------------------

nhs <- readr::read_csv(nhs_file, show_col_types = FALSE)

nut_values <- nhs |>
  count(year, nut, sort = TRUE)

print(nut_values)


life_table_raw <- readxl::read_excel(life_table_file)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

age_band_to_age <- function(x) {
  x <- as.character(x)

  out <- rep(NA_integer_, length(x))
  is_closed_band <- stringr::str_detect(x, "^\\d{2}-\\d{2}$")

  lower <- as.integer(stringr::str_sub(x[is_closed_band], 1L, 2L))
  upper <- as.integer(stringr::str_sub(x[is_closed_band], 4L, 5L))

  out[is_closed_band] <- vapply(
    seq_along(lower),
    function(i) {
      sample.int(
        upper[i] - lower[i] + 1L,
        size = 1L
      ) + lower[i] - 1L
    },
    integer(1)
  )

  out
}

education_3_level <- function(x) {
  dplyr::case_when(
    x %in% c("none", "grade_6") ~ "low",
    x %in% c("grade_9", "grade_12") ~ "medium",
    x == "higher" ~ "high",
    TRUE ~ NA_character_
  )
}

education_2_level <- function(x) {
  dplyr::case_when(
    x %in% c("none", "grade_6", "grade_9", "grade_12") ~ "lower",
    x == "higher" ~ "higher",
    TRUE ~ NA_character_
  )
}


region_calibration <- function(x) {
  x <- stringr::str_to_lower(
    stringr::str_trim(as.character(x))
  )

  dplyr::case_when(
    x == "norte" ~ "north",
    x == "centro" ~ "center",
    x == "lvt" ~ "lisbon_ma",
    x == "alentejo" ~ "alentejo",
    x == "algarve" ~ "algarve",
    x == "madeira" ~ "madeira",
    x %in% c("açores", "acores") ~ "azores",
    TRUE ~ NA_character_
  )
}

unmapped_regions <- nhs |>
  mutate(region = region_calibration(nut)) |>
  filter(is.na(region)) |>
  distinct(nut)

if (nrow(unmapped_regions) > 0L) {
  stop(
    "Unmapped `nut` values: ",
    paste(unmapped_regions$nut, collapse = ", "),
    call. = FALSE
  )
}

# Calibration equations: revision 1 of the calibration paper (Supplementary
# Table S9). Measured height (cm) and weight (kg) are predicted from the
# self-reported value, age and region (North = reference), separately by sex;
# BMI is computed from calibrated weight and height. The equations have no
# education term; `education_level` is kept so existing calls still work.
# The equations used up to simulation v9 (with education) are in
# data-raw/backup_v9_calibration/prepare_model_inputs.R.
calibrate_anthropometry <- function(
    sex,
    age,
    height_sr_cm,
    weight_sr_kg,
    region,
    education_level = NULL
) {

  region <- as.character(region)
  sex <- as.character(sex)

  height_region_effect <- dplyr::case_when(
    sex == "women" & region == "north" ~ 0,
    sex == "women" & region == "center" ~ 0.433,
    sex == "women" & region == "lisbon_ma" ~ 0.699,
    sex == "women" & region == "alentejo" ~ 0.372,
    sex == "women" & region == "algarve" ~ 1.131,
    sex == "women" & region == "madeira" ~ 0.977,
    sex == "women" & region == "azores" ~ 0.373,

    sex == "men" & region == "north" ~ 0,
    sex == "men" & region == "center" ~ -0.065,
    sex == "men" & region == "lisbon_ma" ~ -0.214,
    sex == "men" & region == "alentejo" ~ -0.484,
    sex == "men" & region == "algarve" ~ 0.183,
    sex == "men" & region == "madeira" ~ -0.053,
    sex == "men" & region == "azores" ~ -0.135,
    TRUE ~ NA_real_
  )

  weight_region_effect <- dplyr::case_when(
    sex == "women" & region == "north" ~ 0,
    sex == "women" & region == "center" ~ 0.080,
    sex == "women" & region == "lisbon_ma" ~ 0.121,
    sex == "women" & region == "alentejo" ~ -0.541,
    sex == "women" & region == "algarve" ~ -0.143,
    sex == "women" & region == "madeira" ~ 0.171,
    sex == "women" & region == "azores" ~ 0.002,

    sex == "men" & region == "north" ~ 0,
    sex == "men" & region == "center" ~ -0.725,
    sex == "men" & region == "lisbon_ma" ~ -0.540,
    sex == "men" & region == "alentejo" ~ -0.848,
    sex == "men" & region == "algarve" ~ 0.230,
    sex == "men" & region == "madeira" ~ -0.436,
    sex == "men" & region == "azores" ~ -0.036,
    TRUE ~ NA_real_
  )

  height_calibrated_cm <- dplyr::case_when(
    sex == "women" ~
      20.945 + 0.871 * height_sr_cm - 0.061 * age + height_region_effect,

    sex == "men" ~
      15.480 + 0.913 * height_sr_cm - 0.031 * age + height_region_effect,

    TRUE ~ NA_real_
  )

  weight_calibrated_kg <- dplyr::case_when(
    sex == "women" ~
      -0.670 + 1.014 * weight_sr_kg + 0.010 * age + weight_region_effect,

    sex == "men" ~
      -2.260 + 1.018 * weight_sr_kg + 0.036 * age + weight_region_effect,

    TRUE ~ NA_real_
  )

  tibble::tibble(
    height_calibrated_cm = height_calibrated_cm,
    weight_calibrated_kg = weight_calibrated_kg,
    bmi_calibrated = weight_calibrated_kg /
      (height_calibrated_cm / 100)^2
  )
}


make_donor_pool <- function(data, wave_year) {
  data |>
    filter(
      year == wave_year,
      age_single >= 20L,
      age_single <= 24L
    ) |>
    transmute(
      sex,
      age = as.integer(age_single),
      education_level,
      bmi,
      survey_weight,
      weight = weight_model_kg,
      height = height_model_cm
    )
}

# -----------------------------------------------------------------------------
# Prepare NHS model data
# -----------------------------------------------------------------------------

nhs_model <- nhs |>
  mutate(
    age_single = age_band_to_age(age),
    education_level = education_3_level(edu),
    education_level2 = education_2_level(edu),
    region = region_calibration(nut),
    survey_weight = as.numeric(POND2),
    height_sr_cm = as.numeric(height),
    weight_sr_kg = as.numeric(weight),
    bmi_self_reported = as.numeric(bmi),
    year = as.integer(year),
    sex = as.character(sex)
  ) |>
  mutate(
    calibration = calibrate_anthropometry(
      sex = sex,
      age = age_single,
      height_sr_cm = height_sr_cm,
      weight_sr_kg = weight_sr_kg,
      region = region,
      education_level = education_level
    )
  ) |>
  tidyr::unnest_wider(calibration) |>
  mutate(
    # BMI, weight and height all come from the same scenario, so weight stays
    # consistent with BMI when the model updates it.
    bmi = if (apply_reporting_bias_calibration) {
      bmi_calibrated
    } else {
      bmi_self_reported
    },
    weight_model_kg = if (apply_reporting_bias_calibration) {
      weight_calibrated_kg
    } else {
      weight_sr_kg
    },
    height_model_cm = if (apply_reporting_bias_calibration) {
      height_calibrated_cm
    } else {
      height_sr_cm
    }
  ) |>
  filter(
    age_single >= 20L,
    age_single <= 84L,
    sex %in% c("men", "women"),
    !is.na(education_level),
    !is.na(education_level2),
    !is.na(region),
    !is.na(survey_weight),
    survey_weight > 0,
    !is.na(height_sr_cm),
    !is.na(weight_sr_kg),
    !is.na(bmi),
    bmi >= 10,
    bmi <= 80
  )

calibration_diagnostic <- nhs_model |>
  group_by(year, sex) |>
  summarise(
    n = n(),
    mean_bmi_self_reported = weighted.mean(
      bmi_self_reported,
      w = survey_weight,
      na.rm = TRUE
    ),
    mean_bmi_calibrated = weighted.mean(
      bmi_calibrated,
      w = survey_weight,
      na.rm = TRUE
    ),
    mean_bmi_difference = mean_bmi_calibrated - mean_bmi_self_reported,
    .groups = "drop"
  )

print(calibration_diagnostic)

# Check plausibility of the corrected anthropometric values
calibrated_anthropometry_diagnostic <- nhs_model |>
  summarise(
    n = n(),
    height_calibrated_min = min(height_calibrated_cm, na.rm = TRUE),
    height_calibrated_max = max(height_calibrated_cm, na.rm = TRUE),
    weight_calibrated_min = min(weight_calibrated_kg, na.rm = TRUE),
    weight_calibrated_max = max(weight_calibrated_kg, na.rm = TRUE),
    bmi_calibrated_min = min(bmi_calibrated, na.rm = TRUE),
    bmi_calibrated_max = max(bmi_calibrated, na.rm = TRUE),
    n_invalid_calibrated_height = sum(
      height_calibrated_cm < 100 |
        height_calibrated_cm > 220,
      na.rm = TRUE
    ),
    n_invalid_calibrated_weight = sum(
      weight_calibrated_kg < 20 |
        weight_calibrated_kg > 250,
      na.rm = TRUE
    )
  )

print(calibrated_anthropometry_diagnostic)

# -----------------------------------------------------------------------------
# Baseline population: NHS 1998, ages 20–84
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Baseline population: NHS 1998, ages 20–84
# -----------------------------------------------------------------------------

population_1998 <- nhs_model |>
  filter(year == 1998L) |>
  transmute(
    person_id = paste0("nhs1998_", id),
    age = as.integer(age_single),
    sex,
    education_level,
    education_level2,
    bmi,
    survey_weight,
    weight = weight_model_kg,
    height = height_model_cm
  ) |>
  distinct(person_id, .keep_all = TRUE)

if (nrow(population_1998) == 0L) {
  stop("The NHS 1998 baseline population is empty.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# BMI panel: repeated cross-sectional NHS data
# -----------------------------------------------------------------------------

bmi_panel <- nhs_model |>
  transmute(
    year,
    sex,
    age             = as.integer(age_single),
    education_level,   # 3-level: low / medium / high
    education_level2,  # 2-level: lower / higher (used by BMI-transition model)
    survey_weight,
    bmi
  )

if (!all(c(1998L, 2005L, 2014L, 2019L) %in% unique(bmi_panel$year))) {
  stop(
    "The BMI panel does not contain all required NHS waves: ",
    "1998, 2005, 2014, 2019.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Reference BMI prevalence by Ferreira age band (NHS 2019, ages 20–84)
# Used to back-calculate the healthy-weight EQ-5D utility in QALY module.
# Categories: overweight [25,30), obesity_I [30,35), obesity_II [35,40),
#             obesity_III [40,Inf).  Ferreira bands: 20-29, 30-49, 50-69, 70-84.
# -----------------------------------------------------------------------------

reference_bmi_prevalence <- nhs_model |>
  filter(
    year == 2019L,
    age_single >= 20L,
    age_single <= 84L
  ) |>
  mutate(
    age_band = dplyr::case_when(
      age_single <= 29L ~ "20_29",
      age_single <= 49L ~ "30_49",
      age_single <= 69L ~ "50_69",
      age_single <= 84L ~ "70_84"
    ),
    bmi_category = simeq_bmi_category(bmi)
  ) |>
  filter(!is.na(age_band), !is.na(bmi_category)) |>
  group_by(age_band, bmi_category) |>
  summarise(
    cat_weight = sum(survey_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(age_band) |>
  mutate(prevalence = cat_weight / sum(cat_weight)) |>
  ungroup() |>
  filter(
    bmi_category %in% c("overweight", "obesity_I", "obesity_II", "obesity_III")
  ) |>
  select(age_band, bmi_category, prevalence)

if (!all(c("20_29", "30_49", "50_69", "70_84") %in%
         unique(reference_bmi_prevalence$age_band))) {
  stop(
    "Reference BMI prevalence is missing one or more Ferreira age bands.",
    call. = FALSE
  )
}

print(reference_bmi_prevalence)

# -----------------------------------------------------------------------------
# Portuguese mortality life table
# -----------------------------------------------------------------------------

life_table <- bind_rows(
  life_table_raw |>
    transmute(
      age = as.integer(age),
      sex = "men",
      qx = as.numeric(qx_m) / 1000
    ),
  life_table_raw |>
    transmute(
      age = as.integer(age),
      sex = "women",
      qx = as.numeric(qx_w) / 1000
    )
) |>
  filter(
    age >= 20L,
    age <= 84L
  ) |>
  arrange(sex, age)

# -----------------------------------------------------------------------------
# Entrant donor pools: respondents aged 20–24 by NHS wave
# -----------------------------------------------------------------------------

donor_pool_1998 <- make_donor_pool(nhs_model, 1998L)
donor_pool_2005 <- make_donor_pool(nhs_model, 2005L)
donor_pool_2014 <- make_donor_pool(nhs_model, 2014L)
donor_pool_2019 <- make_donor_pool(nhs_model, 2019L)

donor_pools <- list(
  `1998` = donor_pool_1998,
  `2005` = donor_pool_2005,
  `2014` = donor_pool_2014,
  `2019` = donor_pool_2019
)

if (any(vapply(donor_pools, nrow, integer(1)) == 0L)) {
  empty_waves <- names(donor_pools)[
    vapply(donor_pools, nrow, integer(1)) == 0L
  ]

  stop(
    "Entrant donor pool(s) empty for NHS wave(s): ",
    paste(empty_waves, collapse = ", "),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Model settings
# -----------------------------------------------------------------------------

settings <- simeq_settings(
  initial_year = 1998L,
  projection_end_year = 2040L,
  entry_age = 20L,
  max_age = 84L,
  n_simulations = 100L,
  entrant_n_per_year = 1000L,
  seed = 1234L
)

# -----------------------------------------------------------------------------
# Validate inputs
# -----------------------------------------------------------------------------

simeq_validate_population(
  population = population_1998,
  initial_year = settings$initial_year,
  entry_age = settings$entry_age,
  max_age = settings$max_age
)

simeq_validate_bmi_panel(
  bmi_panel = bmi_panel
)

simeq_validate_life_table(
  life_table = life_table,
  entry_age = settings$entry_age,
  max_age = settings$max_age
)

purrr::walk(
  donor_pools,
  \(pool) simeq_validate_entrant_donor_pool(
    donor_pool = pool,
    entry_age = settings$entry_age
  )
)

# -----------------------------------------------------------------------------
# Diagnostics
# -----------------------------------------------------------------------------

donor_pool_diagnostic <- purrr::imap_dfr(
  donor_pools,
  \(pool, wave_year) tibble::tibble(
    wave_year = as.integer(wave_year),
    n_records = nrow(pool),
    weighted_population = sum(pool$survey_weight, na.rm = TRUE),
    mean_bmi = weighted.mean(
      pool$bmi,
      w = pool$survey_weight,
      na.rm = TRUE
    )
  )
)

print(donor_pool_diagnostic)


# -----------------------------------------------------------------------------
# Save prepared inputs
# -----------------------------------------------------------------------------

inputs <- list(
  population_1998 = population_1998,
  bmi_panel = bmi_panel,
  life_table = life_table,
  donor_pool_1998 = donor_pool_1998,
  donor_pool_2005 = donor_pool_2005,
  donor_pool_2014 = donor_pool_2014,
  donor_pool_2019 = donor_pool_2019,
  settings = settings,
  reference_bmi_prevalence = reference_bmi_prevalence
)

saveRDS(inputs, output_file)

message("Prepared model inputs saved to: ", normalizePath(output_file))

