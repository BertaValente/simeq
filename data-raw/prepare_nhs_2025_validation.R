# -----------------------------------------------------------------------------
# NHS 2025: out-of-sample validation data
# -----------------------------------------------------------------------------
#
# NHS 2025 is kept OUT of the model inputs. prepare_model_inputs.R builds the
# BMI transitions and entrant pools from every wave in its input file, so
# adding 2025 there would fit the model to it. This script cleans 2025 the
# same way as the earlier waves, applies the same self-report calibration
# equations, and writes a separate file that the analysis only compares
# against.
#
# Output: data-raw/nhs_2025_validation.rds, one row per adult aged 20-84,
# with year, sex, age, education_level, education_level2, region,
# survey_weight, bmi (calibrated) and bmi_self_reported.
#
# Check the three marked blocks against the 2025 codebook before running:
# variable names, education codes and region codes can change between waves.

library(dplyr)

# CHECK 0: the simeq project folder (the one with DESCRIPTION and
# data-raw/), found by here::here().
project_root <- here::here()

if (!file.exists(file.path(project_root, "data-raw", "prepare_model_inputs.R"))) {
  stop(
    "data-raw/prepare_model_inputs.R not found under project_root:\n  ",
    project_root,
    call. = FALSE
  )
}

# CHECK 1: the two 2025 files. The QC file holds height, weight, age, sex,
# region and the weight WGT; the MC file adds the other variables
# (including education). They are joined by PID.
survey_dir <- file.path(project_root, "data-raw", "national_health_surveys")
raw_file_qc <- file.path(survey_dir, "INS2025_QC_Fins Científicos_14-09-2026.sav")
raw_file_mc <- file.path(survey_dir, "INS2025_MC_Fins Científicos_13-08-2026.sav")

# Revision-1 calibration equations: new file, so the v9 validation data stay unchanged.
output_file <- file.path(project_root, "data-raw", "nhs_2025_validation_rev1.rds")

# The calibration equations and recoding helpers are taken from
# prepare_model_inputs.R, so both scripts always use identical definitions.
helper_names <- c(
  "age_band_to_age",
  "education_3_level",
  "education_2_level",
  "region_calibration",
  "calibrate_anthropometry"
)

prepare_script <- parse(
  file.path(project_root, "data-raw", "prepare_model_inputs.R"),
  encoding = "UTF-8"
)

for (expression in prepare_script) {
  if (
    is.call(expression) &&
    identical(expression[[1L]], as.name("<-")) &&
    is.name(expression[[2L]]) &&
    as.character(expression[[2L]]) %in% helper_names
  ) {
    eval(expression, envir = globalenv())
  }
}

missing_helpers <- helper_names[!vapply(helper_names, exists, logical(1))]

if (length(missing_helpers) > 0L) {
  stop(
    "Helpers not found in prepare_model_inputs.R: ",
    paste(missing_helpers, collapse = ", "),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Read and recode (same steps as the 2019 cleaning code)
# -----------------------------------------------------------------------------

# Education: ISCED11 in 2025 (HATLEVEL in 2019), same codes:
# -1 no answer, 0 none, 1 basic 1st-2nd cycle, 2 basic 3rd cycle,
# 3 secondary, 4 post-secondary, 5 CTeSP, 6 bachelor/licenciatura,
# 7 pre-Bologna licenciatura or master, 8 doctorate.
education_variable <- "ISCED11"

# Only the needed columns are read: the full files are large and reading
# (or View()-ing) every column can make R appear frozen.
qc_variables <- c(
  "PID", "REFYEAR", "SEX", "AGE_COD", "NUTS2_24", "BM1", "BM2", "WGT"
)

read_columns <- function(path, columns) {
  switch(
    tools::file_ext(path),
    sav = haven::read_sav(path, col_select = dplyr::any_of(columns)),
    dta = haven::read_dta(path, col_select = dplyr::any_of(columns)),
    csv = readr::read_csv(
      path, col_select = dplyr::any_of(columns), show_col_types = FALSE
    ),
    stop("Unsupported file type: ", path, call. = FALSE)
  )
}

ins_2025_qc <- read_columns(raw_file_qc, c(qc_variables, education_variable))

missing_qc <- setdiff(qc_variables, names(ins_2025_qc))

if (length(missing_qc) > 0L) {
  stop(
    "Not found in the QC file: ", paste(missing_qc, collapse = ", "),
    call. = FALSE
  )
}

# Education comes from the MC file unless the QC file already has it.
if (education_variable %in% names(ins_2025_qc)) {
  ins_2025 <- ins_2025_qc
} else {
  ins_2025_mc <- read_columns(raw_file_mc, c("PID", education_variable))

  if (!education_variable %in% names(ins_2025_mc)) {
    stop(
      "Education variable `", education_variable,
      "` not found in either 2025 file.",
      call. = FALSE
    )
  }

  ins_2025 <- left_join(ins_2025_qc, ins_2025_mc, by = "PID")

  message(
    "QC file: ", nrow(ins_2025_qc), " rows; MC file: ", nrow(ins_2025_mc),
    " rows; QC respondents matched on PID: ",
    sum(ins_2025_qc$PID %in% ins_2025_mc$PID), "."
  )
}

# CHECK 2b: the other 2025 variable names.
label_or_value <- function(x) {
  as.character(if (inherits(x, "haven_labelled")) haven::as_factor(x) else x)
}

nhs_2025 <- ins_2025 |>
  transmute(
    id = PID,
    survey_year = as.integer(REFYEAR),
    sex_code = as.integer(SEX),
    age_code = as.integer(AGE_COD),
    edu_code = as.integer(.data[[education_variable]]),
    region_code = as.character(if (inherits(NUTS2_24, "haven_labelled")) haven::zap_labels(NUTS2_24) else NUTS2_24),
    region_label = label_or_value(NUTS2_24),
    height = as.numeric(BM1),
    weight = as.numeric(BM2),
    POND2 = as.numeric(WGT)
  )

# AGE_COD (Grupo etario) codes 1-16 are converted to the band labels used
# in the earlier waves: 1 = 15-17, 2 = 18-19, 3 = 20-24, ..., 15 = 80-84,
# 16 = 85+. A single age within the band is drawn later, as for 2019.
age_bands <- c(
  "15-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">=85"
)

# CHECK 3: education and region codes.
# Education: post-secondary non-tertiary (4, 5) is grouped with 12th grade
# and tertiary (6-8) is "higher". Confirm this matches how the 2019 file
# was recoded to `edu`.
# Region: labels are matched to the seven calibration regions. If 2025 uses
# the 2024 NUTS II regions, Grande Lisboa and Peninsula de Setubal map to the
# Lisbon metropolitan area and Oeste e Vale do Tejo to Centro, as under the
# NUTS 2013 boundaries used to fit the equations.
nhs_2025 <- nhs_2025 |>
  mutate(
    year = 2025L,
    sex = case_when(sex_code == 1L ~ "men", sex_code == 2L ~ "women"),
    age = age_bands[age_code],
    edu = case_when(
      edu_code == 0L ~ "none",
      edu_code == 1L ~ "grade_6",
      edu_code == 2L ~ "grade_9",
      # 4-5 grouped with secondary, as in the 2019 cleaning code.
      edu_code %in% 3:5 ~ "grade_12",
      edu_code %in% 6:8 ~ "higher"
    ),
    nut = {
      label <- stringr::str_to_lower(region_label)
      # Codes may come with or without the "PT" prefix (11, 1A, ...).
      code <- sub("^PT", "", stringr::str_to_upper(stringr::str_trim(region_code)))
      case_when(
        # NUTS 2024 codes. PT1D Oeste e Vale do Tejo was mostly Centro under
        # NUTS 2013 (the boundaries of the calibration equations), and
        # PT1A/PT1B were the Lisbon metropolitan area.
        code == "11" ~ "norte",
        code %in% c("19", "16", "1D") ~ "centro",
        code %in% c("1A", "1B", "17") ~ "lvt",
        code %in% c("1C", "18") ~ "alentejo",
        code == "15" ~ "algarve",
        code == "20" ~ "acores",
        code == "30" ~ "madeira",
        stringr::str_detect(label, "norte") ~ "norte",
        stringr::str_detect(label, "algarve") ~ "algarve",
        stringr::str_detect(label, "alentejo") ~ "alentejo",
        stringr::str_detect(label, "a\u00e7ores|acores|azores") ~ "acores",
        stringr::str_detect(label, "madeira") ~ "madeira",
        stringr::str_detect(label, "lisboa|set\u00fabal|setubal|lisbon") ~ "lvt",
        stringr::str_detect(label, "centro|oeste|vale do tejo") ~ "centro"
      )
    },
    height = if_else(height < 0, NA_real_, height),
    weight = if_else(weight < 0, NA_real_, weight),
    bmi = weight / (height / 100)^2
  )

unmapped <- nhs_2025 |>
  filter(is.na(sex) | is.na(edu) | is.na(nut)) |>
  count(sex_code, edu_code, region_code, region_label)

if (nrow(unmapped) > 0L) {
  message("Rows with unmapped codes (dropped below):")
  print(unmapped, n = Inf)
}

# Complete-case subset after cleaning, all ages, in the same layout as the
# earlier subsets (subset_2019.csv). Kept next to the raw file.
subset_2025 <- nhs_2025 |>
  select(id, year, sex, age, edu, nut, height, weight, bmi, POND2) |>
  filter(
    !is.na(sex), !is.na(age), !is.na(edu), !is.na(nut),
    !is.na(height), !is.na(weight), !is.na(POND2)
  )

message(
  "Complete cases: ", nrow(subset_2025), " of ", nrow(nhs_2025), " respondents."
)

readr::write_csv(
  subset_2025,
  file.path(survey_dir, "subset_2025.csv"),
  na = ""
)

# -----------------------------------------------------------------------------
# Same filters and calibration as prepare_model_inputs.R
# -----------------------------------------------------------------------------

set.seed(20251001L)

nhs_2025_validation <- nhs_2025 |>
  mutate(
    age_single = age_band_to_age(age),
    education_level = education_3_level(edu),
    education_level2 = education_2_level(edu),
    region = region_calibration(nut),
    survey_weight = POND2,
    bmi_self_reported = bmi,
    calibration = calibrate_anthropometry(
      sex = sex,
      age = age_single,
      height_sr_cm = height,
      weight_sr_kg = weight,
      region = region,
      education_level = education_level
    )
  ) |>
  tidyr::unnest_wider(calibration) |>
  filter(
    age_single >= 20L,
    age_single <= 84L,
    sex %in% c("men", "women"),
    !is.na(education_level),
    !is.na(region),
    !is.na(survey_weight),
    survey_weight > 0,
    !is.na(bmi_calibrated),
    bmi_calibrated >= 10,
    bmi_calibrated <= 80
  ) |>
  transmute(
    id,
    year,
    sex,
    age = as.integer(age_single),
    education_level,
    education_level2,
    region,
    survey_weight,
    bmi = bmi_calibrated,
    bmi_self_reported
  )

if (any(nhs_2025$survey_year != 2025L, na.rm = TRUE)) {
  message("REFYEAR values: ", paste(sort(unique(nhs_2025$survey_year)), collapse = ", "))
}

# Quick look: obesity prevalence by sex, calibrated and self-reported.
nhs_2025_validation |>
  group_by(sex) |>
  summarise(
    n = n(),
    obesity_calibrated = weighted.mean(bmi >= 30, survey_weight),
    obesity_self_reported = weighted.mean(bmi_self_reported >= 30, survey_weight),
    .groups = "drop"
  ) |>
  print()

saveRDS(nhs_2025_validation, output_file)
message("Saved: ", output_file)
