# Test of the BMI percentile transitions' upper tail (SIMEQ-PT v10 inputs).
# Does not change the package, the v10 analysis or its results: every output
# goes to results/test_transition_tails/.
#
# Variants (same inputs, seeds and settings; 5 replications, 1998-2025):
#   A  v10 baseline: percentiles P10-P90, sex interactions (control)
#   B  A with the tails added: P5, P10-P90, P95
#   C  B plus a sex x education (2-level) interaction in every percentile model
# Each run is compared with calibrated NHS BMI (revision-1 equations):
# 2005, 2014, 2019 in sample and 2025 out of sample.
# Run from the package root: Rscript data-raw/test_transition_tails.R
fence <- strrep(intToUtf8(96), 3)
lines <- readLines("analysis/simeq_pt_analysis_v10.qmd", encoding = "UTF-8")
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
fit_percentiles <- function(probs) {
  simeq_fit_bmi_transition_percentile(bmi_panel, probs = probs, sex_interactions = TRUE,
                                      life_table = life_table, rr_bmi = rr_bmi, rr_education = rr_education)
}
# Variant C: refit every percentile model from the same cohort data with an
# added sex x education term (the package function has no such option).
add_sex_education <- function(model) {
  periods <- model$periods
  model$models <- lapply(seq_along(model$probs), function(i) {
    m0 <- model$models[[i]]
    d <- model$cohorts[abs(model$cohorts$prob - model$probs[i]) < 1e-9, , drop = FALSE]
    d$sex <- factor(d$sex, levels = c("men", "women"))
    d$education_level2 <- factor(d$education_level2)
    d$period <- factor(d$period, levels = periods)
    stats::lm(stats::update(formula(m0), . ~ . + sex:education_level2), data = d, weights = transition_weight)
  })
  model
}
models <- list(
  A = bmi_percentile_model,
  B = fit_percentiles(c(0.05, seq(0.1, 0.9, by = 0.1), 0.95))
)
models$C <- add_sex_education(models$B)
stopifnot(isTRUE(all.equal(lapply(models$A$models, coef),
                           lapply(fit_percentiles(seq(0.1, 0.9, by = 0.1))$models, coef))))

# ---- Simulations ---------------------------------------------------------------
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
  sim[sim$year %in% c(2005L, 2014L, 2019L, 2025L) & sim$alive & sim$age >= 20 & sim$age <= 84,
      c("simulation", "year", "sex", "age", "education_level", "survey_weight", "bmi")]
}
sims <- lapply(names(models), function(v) {
  t0 <- Sys.time(); s <- simulate(models[[v]])
  message("Variant ", v, ": ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
  s$variant <- v; s
})
sims <- bind_rows(sims) |>
  mutate(sex = as.character(sex), education_level = as.character(education_level))

# ---- Comparison with calibrated NHS -------------------------------------------
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
observed <- bind_rows(
  readRDS("data-raw/model_inputs_prepared_reporting_bias_calibrated_rev1.rds")$bmi_panel |>
    filter(year %in% c(2005L, 2014L, 2019L)) |> select(year, sex, age, education_level, survey_weight, bmi),
  readRDS("data-raw/nhs_2025_validation_rev1.rds") |>
    filter(age >= 20, age <= 84) |> select(year, sex, age, education_level, survey_weight, bmi)
) |> mutate(sex = as.character(sex), education_level = as.character(education_level), age_group = age_group(age))
sims$age_group <- age_group(sims$age)

compare <- function(by) {
  obs <- describe(observed, c("year", by)) |> pivot_longer(all_of(stats), names_to = "stat", values_to = "observed")
  describe(sims, c("variant", "simulation", "year", by)) |>
    pivot_longer(all_of(stats), names_to = "stat") |>
    group_by(across(all_of(c("variant", "year", by, "stat")))) |>
    summarise(simulated = mean(value), .groups = "drop") |>
    left_join(obs, by = c("year", by, "stat")) |>
    mutate(diff = simulated - observed)
}
by_sex <- compare("sex")
by_sex_age <- compare(c("sex", "age_group")) |> filter(!is.na(age_group))
by_sex_edu <- compare(c("sex", "education_level")) |> filter(education_level %in% c("low", "medium", "high"))
write_csv(by_sex, file.path(out_dir, "comparison_by_sex.csv"))
write_csv(by_sex_age, file.path(out_dir, "comparison_by_sex_age.csv"))
write_csv(by_sex_edu, file.path(out_dir, "comparison_by_sex_education.csv"))

errors <- by_sex |>
  filter(stat == "obesity") |>
  group_by(variant) |>
  summarise(mae_in_sample = mean(abs(diff[year < 2025])), rmse_in_sample = sqrt(mean(diff[year < 2025]^2)),
            women_2025 = diff[year == 2025 & sex == "women"], men_2025 = diff[year == 2025 & sex == "men"],
            .groups = "drop")
write_csv(errors, file.path(out_dir, "obesity_errors.csv"))

# Percentile figure: observed vs the three variants, by sex and year.
fig <- by_sex |>
  filter(stat %in% names(probs)) |>
  mutate(stat = factor(toupper(stat), levels = toupper(names(probs))),
         sex = factor(tools::toTitleCase(sex), levels = c("Women", "Men")),
         year = ifelse(year == 2025L, "2025 (out of sample)", as.character(year))) |>
  ggplot(aes(stat, diff, colour = variant, group = variant)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.6) +
  facet_grid(sex ~ year) +
  scale_colour_manual(values = c(A = "grey40", B = "#2C7FB8", C = "#D55E00"),
                      labels = c(A = "A: v10 (P10-P90)", B = "B: + P5 and P95", C = "C: B + sex x education")) +
  labs(x = "BMI percentile", y = "Simulated minus observed BMI (kg/m²)", colour = NULL,
       caption = "5 replications per variant; calibrated NHS BMI, ages 20-84.") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom", strip.background = element_blank(), strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3), plot.caption = element_text(hjust = 0))
ggsave(file.path(out_dir, "percentile_error_by_variant.png"), fig, width = 7.5, height = 4.6, dpi = 300)

options(width = 220)
wide <- function(d, by, keep) {
  d |> filter(stat %in% keep) |>
    mutate(txt = sprintf("%+.2f", diff)) |>
    select(all_of(c("year", by, "stat", "variant")), txt) |>
    pivot_wider(names_from = c(stat, variant), values_from = txt) |> as.data.frame() |> print(row.names = FALSE)
}
cat("\nObesity error summary (pp)\n"); print(as.data.frame(errors), digits = 3)
cat("\nSimulated minus observed, by sex\n"); wide(by_sex, "sex", c("obesity", "sd", "p75", "p90", "p95"))
cat("\nObesity error by sex x age group (pp)\n"); wide(by_sex_age, c("sex", "age_group"), "obesity")
cat("\nObesity error by sex x education (pp)\n"); wide(by_sex_edu, c("sex", "education_level"), "obesity")
