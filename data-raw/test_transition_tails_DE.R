# Second transition test (SIMEQ-PT v10 inputs). Does not change the package,
# the v10 analysis or its results: outputs go to results/test_transition_tails/.
#
# Variants (same inputs, seeds and settings as data-raw/test_transition_tails.R;
# 5 replications, 1998-2025):
#   A  v10 baseline (control; identical to variant A of the first test)
#   D  A without survival weighting of the synthetic-cohort percentiles
#   E  A with 5-year age-group effects by sex instead of age + age^2
# Plus a diagnostic of the 1998 -> 2005 build-up of the gap in women 50-64:
# observed synthetic-cohort change vs the change the simulation (A) produces
# for the same cohort, and the 2005 education composition.
# Run from the package root: Rscript data-raw/test_transition_tails_DE.R
fence <- strrep(intToUtf8(96), 3)
lines <- readLines("simeq_analysis_clean_lc_v6.qmd", encoding = "UTF-8")
chunk_code <- function(label) {
  s <- which(lines == paste0("#| label: ", label))
  e <- which(trimws(lines) == fence); e <- e[e > s][1]
  code <- lines[(s + 1):(e - 1)]
  code[!startsWith(code, "#|")]
}
run_chunk <- function(label) eval(parse(text = chunk_code(label)), envir = globalenv())

run_chunk("setup")
n_simulations_main <- 5L
for (lab in c("prepare-inputs", "build-rr-bmi", "build-rr-education", "build-bmi-model", "settings-module1")) run_chunk(lab)
settings_historical$projection_end_year <- 2025L
run_chunk("entrant-targets-module1")
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr); library(ggplot2) })

out_dir <- file.path("results", "test_transition_tails")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
donor_pools_module1 <- list(
  list(from = 1999L, to = 2004L, pool = donor_pool_1998),
  list(from = 2005L, to = 2013L, pool = donor_pool_2005),
  list(from = 2014L, to = 2018L, pool = donor_pool_2014),
  list(from = 2019L, to = 2040L, pool = donor_pool_2019)
)

# ---- Transition models -------------------------------------------------------
# E: refit each percentile model with 5-year age-group effects (by sex). The
# groups are cut from the cohort's mean baseline age, so prediction for a
# simulated person (newdata with age_t) uses the same groups; ages above 75
# take the oldest group.
age_breaks <- c(-Inf, seq(25, 75, by = 5), Inf)
age_groups_by_sex <- function(model) {
  periods <- model$periods
  form <- annual_bmi_change ~ period + education_level2 + sex + cut(age_t, breaks = age_breaks) +
    sex:period + sex:cut(age_t, breaks = age_breaks)
  model$models <- lapply(model$probs, function(p) {
    d <- model$cohorts[abs(model$cohorts$prob - p) < 1e-9, , drop = FALSE]
    d$sex <- factor(d$sex, levels = c("men", "women"))
    d$education_level2 <- factor(d$education_level2)
    d$period <- factor(d$period, levels = periods)
    stats::lm(form, data = d, weights = transition_weight)
  })
  model
}
models <- list(
  A = bmi_percentile_model,
  D = simeq_fit_bmi_transition_percentile(bmi_panel, probs = seq(0.1, 0.9, by = 0.1), sex_interactions = TRUE),
  E = age_groups_by_sex(bmi_percentile_model)
)

# ---- Simulations ---------------------------------------------------------------
keep_years <- c(1998L, 2005L, 2014L, 2019L, 2025L)
simulate <- function(percentile_model) {
  sim <- simeq_run_simulation(
    population = population_1998, settings = settings_historical, life_table = life_table,
    last_observed_year = last_observed_life_table_year, rr_bmi = rr_bmi, rr_education = rr_education,
    donor_pool = donor_pool_1998, donor_pools = donor_pools_module1,
    entrant_targets = entrant_targets_historical, bmi_transition_model = bmi_transition_model,
    bmi_update_method = "percentile", bmi_percentile_model = percentile_model, bmi_period = "by_year",
    stochastic_bmi = TRUE, education_source = "existing", bmi_limits = c(10, 80),
    education_upgrade = education_upgrade
  )
  sim[sim$year %in% keep_years & sim$age >= 20 & sim$age <= 84,
      c("simulation", "person_id", "year", "alive", "sex", "age", "education_level", "survey_weight", "bmi")]
}
sims <- lapply(names(models), function(v) {
  t0 <- Sys.time(); s <- simulate(models[[v]])
  message("Variant ", v, ": ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
  s$variant <- v; s
})
sims <- bind_rows(sims) |> mutate(sex = as.character(sex), education_level = as.character(education_level))

# ---- Comparison with calibrated NHS (as in the first test) ---------------------
probs <- c(p50 = 0.50, p75 = 0.75, p90 = 0.90, p95 = 0.95)
stats <- c("mean", "sd", "obesity", names(probs))
age_group <- function(age) cut(age, c(19, 34, 49, 64, 84), labels = c("20-34", "35-49", "50-64", "65-84"))
wq <- function(x, w, p) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(q) x[which(cw >= q)[1]], numeric(1))
}
describe <- function(d, by) {
  d |>
    group_by(across(all_of(by))) |>
    summarise(mean = weighted.mean(bmi, survey_weight),
              sd = sqrt(sum(survey_weight * (bmi - weighted.mean(bmi, survey_weight))^2) / sum(survey_weight)),
              obesity = 100 * sum(survey_weight * (bmi >= 30)) / sum(survey_weight),
              q = list(setNames(wq(bmi, survey_weight, probs), names(probs))), .groups = "drop") |>
    unnest_wider(q)
}
panel_all <- readRDS("data-raw/model_inputs_prepared_reporting_bias_calibrated_rev1.rds")$bmi_panel
observed <- bind_rows(
  panel_all |> filter(year %in% c(2005L, 2014L, 2019L)) |> select(year, sex, age, education_level, survey_weight, bmi),
  readRDS("data-raw/nhs_2025_validation_rev1.rds") |>
    filter(age >= 20, age <= 84) |> select(year, sex, age, education_level, survey_weight, bmi)
) |> mutate(sex = as.character(sex), education_level = as.character(education_level), age_group = age_group(age))
sims_alive <- sims |> filter(alive, year >= 2005L) |> mutate(age_group = age_group(age))

compare <- function(by) {
  obs <- describe(observed, c("year", by)) |> pivot_longer(all_of(stats), names_to = "stat", values_to = "observed")
  describe(sims_alive, c("variant", "simulation", "year", by)) |>
    pivot_longer(all_of(stats), names_to = "stat") |>
    group_by(across(all_of(c("variant", "year", by, "stat")))) |>
    summarise(simulated = mean(value), .groups = "drop") |>
    left_join(obs, by = c("year", by, "stat")) |>
    mutate(diff = simulated - observed)
}
by_sex <- compare("sex")
by_sex_age <- compare(c("sex", "age_group")) |> filter(!is.na(age_group))
by_sex_edu <- compare(c("sex", "education_level")) |> filter(education_level %in% c("low", "medium", "high"))
write_csv(by_sex, file.path(out_dir, "comparison_ADE_by_sex.csv"))
write_csv(by_sex_age, file.path(out_dir, "comparison_ADE_by_sex_age.csv"))
write_csv(by_sex_edu, file.path(out_dir, "comparison_ADE_by_sex_education.csv"))
errors <- by_sex |>
  filter(stat == "obesity") |>
  group_by(variant) |>
  summarise(mae_in_sample = mean(abs(diff[year < 2025])), rmse_in_sample = sqrt(mean(diff[year < 2025]^2)),
            women_2025 = diff[year == 2025 & sex == "women"], men_2025 = diff[year == 2025 & sex == "men"],
            .groups = "drop")
write_csv(errors, file.path(out_dir, "obesity_errors_ADE.csv"))

# ---- 1998 -> 2005 build-up in women ---------------------------------------------
# Observed synthetic cohort: women aged a-7 in the 1998 NHS vs women aged a in
# the 2005 NHS, for a = 50-64 (and 45-54, 55-64); raw and survival-weighted
# baseline. Simulated: the 1998 women of the same ages (variant A), their
# percentiles in 1998 and, among survivors, in 2005.
bands <- list(`50-64` = c(50, 64), `45-54` = c(45, 54), `55-64` = c(55, 64))
# Survival as in the transition fit (whole wave, so the band means match).
wave98 <- panel_all |> filter(year == 1998L)
wave98$w_surv <- wave98$survey_weight * simeq:::.interval_survival(
  wave = wave98, start_year = 1998L, interval = 7L, life_table = life_table,
  rr_bmi = rr_bmi, rr_education = rr_education, max_age = 84L)
women98 <- wave98 |> filter(sex == "women")
women05 <- panel_all |> filter(year == 2005L, sex == "women")
pct <- function(bmi, w) c(wq(bmi, w, c(0.5, 0.75, 0.9)), 100 * sum(w * (bmi >= 30)) / sum(w))
simA <- sims |> filter(variant == "A", sex == "women")
buildup <- bind_rows(lapply(names(bands), function(b) {
  r <- bands[[b]]
  base <- women98 |> filter(age >= r[1] - 7, age <= r[2] - 7)
  fu <- women05 |> filter(age >= r[1], age <= r[2])
  ids <- simA |> filter(year == 1998L, age >= r[1] - 7, age <= r[2] - 7)
  s98 <- ids |> group_by(simulation) |> summarise(v = list(pct(bmi, survey_weight)), .groups = "drop")
  s05 <- simA |> filter(year == 2005L, alive) |>
    semi_join(ids, by = c("simulation", "person_id")) |>
    group_by(simulation) |> summarise(v = list(pct(bmi, survey_weight)), .groups = "drop")
  m98 <- Reduce(`+`, s98$v) / nrow(s98); m05 <- Reduce(`+`, s05$v) / nrow(s05)
  tibble(band_2005 = b, stat = c("P50", "P75", "P90", "obesity %"),
         nhs_1998_raw = pct(base$bmi, base$survey_weight),
         nhs_1998_survivors = pct(base$bmi, base$w_surv),
         nhs_2005 = pct(fu$bmi, fu$survey_weight),
         sim_1998 = m98, sim_2005 = m05) |>
    mutate(obs_change = nhs_2005 - nhs_1998_survivors, sim_change = sim_2005 - sim_1998,
           obs_change_raw = nhs_2005 - nhs_1998_raw)
}))
write_csv(buildup, file.path(out_dir, "buildup_women_1998_2005.csv"))

# 2005 education composition and obesity by education, women 50-64.
comp_obs <- women05 |> filter(age >= 50, age <= 64) |> group_by(education_level) |>
  summarise(w = sum(survey_weight), ob = 100 * sum(survey_weight * (bmi >= 30)) / sum(survey_weight), .groups = "drop") |>
  mutate(share = 100 * w / sum(w), source = "NHS 2005")
comp_sim <- simA |> filter(year == 2005L, alive, age >= 50, age <= 64) |> group_by(simulation, education_level) |>
  summarise(w = sum(survey_weight), ob = 100 * sum(survey_weight * (bmi >= 30)) / sum(survey_weight), .groups = "drop") |>
  group_by(simulation) |> mutate(share = 100 * w / sum(w)) |>
  group_by(education_level) |> summarise(share = mean(share), ob = mean(ob), .groups = "drop") |>
  mutate(source = "Simulated 2005 (A)")
composition <- bind_rows(comp_obs |> select(source, education_level, share, ob), comp_sim)
write_csv(composition, file.path(out_dir, "composition_women_50_64_2005.csv"))

options(width = 220)
cat("\nObesity error summary (pp)\n"); print(as.data.frame(errors), digits = 3)
wide <- function(d, by, keep) {
  d |> filter(stat %in% keep) |> mutate(txt = sprintf("%+.2f", diff)) |>
    select(all_of(c("year", by, "stat", "variant")), txt) |>
    pivot_wider(names_from = c(stat, variant), values_from = txt) |> as.data.frame() |> print(row.names = FALSE)
}
cat("\nSimulated minus observed, by sex\n"); wide(by_sex, "sex", c("obesity", "p75", "p90", "p95", "sd"))
cat("\nObesity error, women by age group (pp)\n"); wide(filter(by_sex_age, sex == "women"), c("sex", "age_group"), "obesity")
cat("\nObesity error, men by age group (pp)\n"); wide(filter(by_sex_age, sex == "men"), c("sex", "age_group"), "obesity")
cat("\nObesity error, women by education (pp)\n"); wide(filter(by_sex_edu, sex == "women"), c("sex", "education_level"), "obesity")
cat("\nBuild-up 1998 -> 2005, women\n"); print(as.data.frame(buildup), digits = 3, row.names = FALSE)
cat("\nWomen 50-64 in 2005: education share and obesity\n"); print(as.data.frame(composition), digits = 3, row.names = FALSE)
