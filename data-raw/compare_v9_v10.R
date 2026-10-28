# Compares SIMEQ-PT status-quo runs v9 (calibration equations with education)
# and v10 (revision-1 equations: age + region), and calibrated vs self-reported
# obesity prevalence in the NHS waves under both sets of equations.
# Run from the package root: Rscript data-raw/compare_v9_v10.R
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr) })

versions <- c(v9 = "v_monte_carlo_n50_age84_calibrated_bmi_v9_lc_edu3",
              v10 = "v_monte_carlo_n50_age84_calibrated_bmi_v10_lc_edu3")
years <- c(2019L, 2027L, 2035L)
out_dir <- file.path("results", "comparison_v9_v10")
dir.create(out_dir, showWarnings = FALSE)

# Obesity (BMI >= 30) prevalence by sex and education among living adults
# aged 20-84 (and 20-64), per replication, then mean and 95% SI.
summarise_run <- function(v) {
  f <- file.path("results", paste0("module1_simulation_1998_2040_", versions[[v]], "_nsim50.rds"))
  d <- readRDS(f)
  d <- d[d$year %in% years & d$alive & d$age >= 20 & d$age <= 84,
         c("simulation", "year", "age", "sex", "education_level", "survey_weight", "bmi")]
  d <- bind_rows(d |> mutate(ages = "20-84"), d |> filter(age <= 64) |> mutate(ages = "20-64")) |>
    mutate(edu = as.character(education_level), sex = as.character(sex))
  invisible(gc())
  tot <- d |>
    group_by(ages, simulation, year, sex) |>
    summarise(all = 100 * sum(survey_weight * (bmi >= 30)) / sum(survey_weight), .groups = "drop")
  out <- d |>
    filter(edu %in% c("low", "medium", "high")) |>
    group_by(ages, simulation, year, sex, edu) |>
    summarise(prev = 100 * sum(survey_weight * (bmi >= 30)) / sum(survey_weight), .groups = "drop")
  rm(d); invisible(gc())
  out |>
    pivot_wider(names_from = edu, values_from = prev) |>
    left_join(tot, by = c("ages", "simulation", "year", "sex")) |>
    mutate(pd = low - high, version = v)
}
sims <- bind_rows(summarise_run("v9"), summarise_run("v10"))
si <- function(x) sprintf("%.1f (%.1f-%.1f)", mean(x), quantile(x, 0.025), quantile(x, 0.975))
comparison <- sims |>
  group_by(version, ages, year, sex) |>
  summarise(obesity_all = si(all), obesity_low = si(low), obesity_high = si(high),
            pd_low_high = si(pd), .groups = "drop") |>
  arrange(ages, year, sex, desc(version))
write_csv(comparison, file.path(out_dir, "obesity_prevalence_v9_v10.csv"))
print(as.data.frame(comparison), right = FALSE)

# Calibrated vs self-reported obesity prevalence in the NHS waves (ages 20-84),
# with the v9 and v10 equations.
prev_inputs <- function(file, label) {
  p <- readRDS(file.path("data-raw", file))$bmi_panel
  p |> mutate(sex = as.character(sex)) |>
    group_by(year, sex) |>
    summarise(obesity = 100 * sum(survey_weight * (bmi >= 30)) / sum(survey_weight), .groups = "drop") |>
    mutate(equations = label)
}
source_env <- new.env()
prep <- parse("data-raw/prepare_model_inputs.R", encoding = "UTF-8")
for (e in prep) if (is.call(e) && identical(e[[1]], as.name("<-")) &&
                    as.character(e[[2]]) %in% c("age_band_to_age", "education_3_level", "region_calibration")) eval(e, source_env)
nhs <- read_csv(file.path("data-raw", "national_health_surveys", "subset_final_with_use_costs_2019.csv"), show_col_types = FALSE)
set.seed(123L)
nhs <- nhs |>
  mutate(age_single = source_env$age_band_to_age(age), sex = as.character(sex), w = as.numeric(POND2),
         bmi_sr = as.numeric(bmi), edu = source_env$education_3_level(edu), region = source_env$region_calibration(nut)) |>
  filter(age_single >= 20, age_single <= 84, sex %in% c("men", "women"), !is.na(edu), !is.na(region),
         !is.na(w), w > 0, !is.na(bmi_sr), bmi_sr >= 10, bmi_sr <= 80)
self_reported <- nhs |>
  group_by(year, sex) |>
  summarise(obesity = 100 * sum(w * (bmi_sr >= 30)) / sum(w), .groups = "drop") |>
  mutate(equations = "self-reported")
waves <- bind_rows(self_reported,
                   prev_inputs("model_inputs_prepared_reporting_bias_calibrated.rds", "calibrated v9 (with education)"),
                   prev_inputs("model_inputs_prepared_reporting_bias_calibrated_rev1.rds", "calibrated v10 (age + region)")) |>
  mutate(obesity = round(obesity, 1)) |>
  pivot_wider(names_from = equations, values_from = obesity) |>
  arrange(year, sex)
write_csv(waves, file.path(out_dir, "nhs_waves_calibrated_vs_self_reported.csv"))
print(as.data.frame(waves))
