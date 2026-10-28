# Diagnostic: is the simulated BMI distribution too narrow? Compares simulated
# (SIMEQ-PT v10 status quo; mean and 95% SI across 50 replications) and
# observed calibrated NHS BMI (revision-1 equations) percentiles, SD and
# obesity prevalence by sex, sex x education and sex x age group, in 2005,
# 2014, 2019 (waves used in fitting) and 2025 (out of sample). Ages 20-84,
# survey-weighted. Read-only: writes CSVs and one figure to
# results/diagnostics_v10_percentiles/. Run from the package root.
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr); library(ggplot2) })

years <- c(2005L, 2014L, 2019L, 2025L)
probs <- c(p50 = 0.50, p75 = 0.75, p90 = 0.90, p95 = 0.95)
stats <- c("mean", "sd", "obesity", names(probs))
out_dir <- file.path("results", "diagnostics_v10_percentiles")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

age_group <- function(age) cut(age, c(19, 34, 49, 64, 84), labels = c("20-34", "35-49", "50-64", "65-84"))
wq <- function(x, w, p) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(q) x[which(cw >= q)[1]], numeric(1))
}
describe <- function(d, by) {
  d |>
    group_by(across(all_of(by))) |>
    summarise(n = n(),
              mean = weighted.mean(bmi, survey_weight),
              sd = sqrt(sum(survey_weight * (bmi - weighted.mean(bmi, survey_weight))^2) / sum(survey_weight)),
              obesity = 100 * sum(survey_weight * (bmi >= 30)) / sum(survey_weight),
              q = list(setNames(wq(bmi, survey_weight, probs), names(probs))), .groups = "drop") |>
    unnest_wider(q)
}

# Observed: calibrated NHS waves and NHS 2025.
panel <- readRDS("data-raw/model_inputs_prepared_reporting_bias_calibrated_rev1.rds")$bmi_panel |>
  filter(year %in% years) |> select(year, sex, age, education_level, survey_weight, bmi)
nhs25 <- readRDS("data-raw/nhs_2025_validation_rev1.rds") |>
  filter(age >= 20, age <= 84) |> select(year, sex, age, education_level, survey_weight, bmi)
observed <- bind_rows(panel, nhs25) |>
  mutate(sex = as.character(sex), education_level = as.character(education_level), age_group = age_group(age))

# Simulated: v10 status quo, living adults 20-84.
sim <- readRDS("results/module1_simulation_1998_2040_v_monte_carlo_n50_age84_calibrated_bmi_v10_lc_edu3_nsim50.rds")
sim <- sim[sim$year %in% years & sim$alive & sim$age >= 20 & sim$age <= 84,
           c("simulation", "year", "sex", "age", "education_level", "survey_weight", "bmi")]
sim$sex <- as.character(sim$sex); sim$education_level <- as.character(sim$education_level)
sim$age_group <- age_group(sim$age)
invisible(gc())

compare <- function(by) {
  obs <- describe(observed, c("year", by)) |> select(-n) |>
    pivot_longer(all_of(stats), names_to = "stat", values_to = "observed")
  s <- describe(sim, c("simulation", "year", by)) |>
    pivot_longer(all_of(stats), names_to = "stat") |>
    group_by(across(all_of(c("year", by, "stat")))) |>
    summarise(simulated = mean(value), sim_lcl = quantile(value, 0.025), sim_ucl = quantile(value, 0.975),
              .groups = "drop")
  left_join(s, obs, by = c("year", by, "stat")) |>
    mutate(diff = simulated - observed, outside_si = observed < sim_lcl | observed > sim_ucl)
}
by_sex <- compare("sex")
by_sex_edu <- compare(c("sex", "education_level")) |> filter(education_level %in% c("low", "medium", "high"))
by_sex_age <- compare(c("sex", "age_group")) |> filter(!is.na(age_group))
write_csv(by_sex, file.path(out_dir, "bmi_percentiles_by_sex.csv"))
write_csv(by_sex_edu, file.path(out_dir, "bmi_percentiles_by_sex_education.csv"))
write_csv(by_sex_age, file.path(out_dir, "bmi_percentiles_by_sex_age.csv"))

# Figure: simulated (point, 95% SI) vs observed (cross) percentiles by sex and year.
fig_data <- by_sex |>
  filter(stat %in% names(probs)) |>
  mutate(stat = factor(toupper(stat), levels = toupper(names(probs))),
         sex = factor(tools::toTitleCase(sex), levels = c("Women", "Men")),
         year = factor(ifelse(year == 2025L, "2025 (out of sample)", as.character(year))))
fig <- ggplot(fig_data, aes(stat)) +
  geom_errorbar(aes(ymin = sim_lcl, ymax = sim_ucl, colour = "Simulated (95% SI)"), width = 0.2) +
  geom_point(aes(y = simulated, colour = "Simulated (95% SI)"), size = 1.8) +
  geom_point(aes(y = observed, colour = "Observed NHS (calibrated)"), shape = 4, size = 2.4, stroke = 0.9) +
  geom_hline(yintercept = 30, linetype = "22", linewidth = 0.3, colour = "grey50") +
  facet_grid(sex ~ year) +
  scale_colour_manual(values = c("Simulated (95% SI)" = "#2C7FB8", "Observed NHS (calibrated)" = "#D55E00")) +
  labs(x = "BMI percentile", y = "BMI (kg/m²)", colour = NULL,
       caption = "Ages 20-84, survey-weighted. Dashed line: BMI 30. SIMEQ-PT v10 status quo, 50 replications.") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom", strip.background = element_blank(), strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3), plot.caption = element_text(hjust = 0))
ggsave(file.path(out_dir, "bmi_percentiles_simulated_vs_observed.png"), fig, width = 7.5, height = 4.6, dpi = 300)

options(width = 220)
show <- function(d, by, keep = stats) {
  d |> filter(stat %in% keep) |>
    mutate(txt = sprintf("%.1f/%.1f%s", simulated, observed, ifelse(outside_si, "*", ""))) |>
    select(all_of(c("year", by)), stat, txt) |>
    pivot_wider(names_from = stat, values_from = txt) |> as.data.frame() |> print(row.names = FALSE)
}
cat("\nSimulated/observed (* observed outside 95% SI), by sex\n"); show(by_sex, "sex")
cat("\nBy sex x education\n"); show(by_sex_edu, c("sex", "education_level"), c("obesity", "p75", "p90", "p95"))
cat("\nBy sex x age group\n"); show(by_sex_age, c("sex", "age_group"), c("obesity", "p75", "p90", "p95"))
