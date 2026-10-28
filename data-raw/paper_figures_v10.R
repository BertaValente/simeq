# Paper figures from the SIMEQ-PT v10 results (status quo, 50 replications).
#   Figure 2:  simulated mean BMI by sex and education, 1998-2040
#   Figure S1: obesity-attributable YLL per 100,000 by sex and education, 1999-2040
# Same plotting code as simeq_analysis_clean_lc_v6.qmd. Writes PNG (200 dpi) and
# TIFF (300 dpi) to results/paper_figures_v10/. Run from the package root.
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

version <- "v_monte_carlo_n50_age84_calibrated_bmi_v10_lc_edu3_nsim50"
out_dir <- file.path("results", "paper_figures_v10")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pub_theme <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
      strip.background   = element_rect(fill = "grey95", colour = NA),
      strip.text         = element_text(size = base_size),
      axis.line          = element_line(colour = "black", linewidth = 0.4),
      axis.ticks         = element_line(colour = "black", linewidth = 0.4),
      legend.title       = element_text(size = base_size),
      legend.text        = element_text(size = base_size - 1L),
      legend.key.width   = unit(1.5, "lines"),
      plot.background    = element_rect(fill = "white", colour = NA)
    )
}
edu_levels <- c(low = "Low education", medium = "Medium education", high = "High education")
save_figure <- function(plot, name) {
  ggsave(file.path(out_dir, paste0(name, ".png")), plot, width = 8.64, height = 4.8, dpi = 200, bg = "white")
  ggsave(file.path(out_dir, paste0(name, ".tiff")), plot, width = 8.64, height = 4.8, dpi = 300, bg = "white",
         compression = "lzw")
}

# ---- Figure 2: mean BMI ----------------------------------------------------------
sim <- as.data.table(readRDS(file.path("results", paste0("module1_simulation_1998_2040_", version, ".rds"))))
sim <- sim[alive == TRUE & age >= 20 & age <= 84, .(simulation, year, sex, education_level, bmi, survey_weight)]
by_rep <- sim[, .(mean_bmi = sum(bmi * survey_weight) / sum(survey_weight)),
              by = .(simulation, year, sex, education_level)]
rm(sim); invisible(gc())
bmi <- by_rep[, .(mean_bmi = mean(mean_bmi), bmi_lo = quantile(mean_bmi, 0.025), bmi_hi = quantile(mean_bmi, 0.975)),
              by = .(year, sex, education_level)]
bmi[, sex := factor(sex, levels = c("women", "men"), labels = c("Women", "Men"))]
bmi[, education_level := factor(edu_levels[as.character(education_level)], levels = edu_levels)]
fwrite(bmi, file.path(out_dir, "Figure2_mean_BMI_v10_data.csv"))

fig2 <- ggplot(bmi, aes(year, mean_bmi, colour = education_level, fill = education_level)) +
  geom_ribbon(aes(ymin = bmi_lo, ymax = bmi_hi), alpha = 0.40, colour = NA, show.legend = FALSE) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", linewidth = 0.5, colour = "grey45") +
  geom_line(linewidth = 0.95) +
  facet_wrap(~ sex, ncol = 2) +
  scale_x_continuous(breaks = seq(2000, 2040, by = 10), limits = c(1998, 2040),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(name = expression("Mean BMI (" * kg / m^2 * ")"), breaks = seq(20, 30, by = 2),
                     limits = c(20, 30), expand = expansion(mult = c(0, 0.02))) +
  scale_colour_manual(values = c("#1B9E77", "#D95F02", "#7570B3"), name = "Education level") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3"), name = "Education level") +
  labs(x = "Year") +
  pub_theme() +
  theme(legend.position = "bottom", panel.spacing.x = unit(2, "lines"))
save_figure(fig2, "Figure2_mean_BMI_v10")

# ---- Figure S1: obesity-attributable YLL -------------------------------------------
burden <- readRDS(file.path("results", paste0("module2_obesity_burden_", version, ".rds")))
yll <- as.data.table(burden$by_year)[year >= 1999L,
  .(year, sex, education_level, yll_obesity_per100k, yll_obesity_per100k_lo, yll_obesity_per100k_hi)]
yll[, sex := factor(sex, levels = c("women", "men"), labels = c("Women", "Men"))]
yll[, education_level := factor(edu_levels[as.character(education_level)], levels = edu_levels)]
fwrite(yll, file.path(out_dir, "FigureS1_YLL_v10_data.csv"))

figs1 <- ggplot(yll, aes(year, yll_obesity_per100k, colour = education_level, fill = education_level)) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", colour = "grey45", linewidth = 0.5) +
  geom_ribbon(aes(ymin = yll_obesity_per100k_lo, ymax = yll_obesity_per100k_hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ sex, ncol = 2) +
  scale_colour_manual(values = c("#C0392B", "#E67E22", "#2E8B57"), name = "Education level") +
  scale_fill_manual(values = c("#C0392B", "#E67E22", "#2E8B57"), name = "Education level") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = "Year", y = "YLL per 100,000") +
  pub_theme() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
save_figure(figs1, "FigureS1_YLL_v10")
