# Module 1: Inputs -> mortality RRs -> BMI model -> historical validation ->
# 2020-2040 projection -> demographic validation/calibration -> obesity outputs
# Module 2: YLL -> QALYs -> YLD/DALYs, only if a disability-weight specification exists
# Module 3: Obesity-attributable costs 2018-2040, only if Borges cost parameters
# and price-year/discount assumptions are fully specified
#
# Outputs are saved to analysis/results/. The Quarto report should read these files.

# analysis/run_full_analysis.R

# =============================================================================
# PONTE-PT-O: Full analysis runner
# =============================================================================

rm(list = ls())

set.seed(123)

devtools::load_all(".")

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(purrr)
library(ggplot2)

results_dir <- file.path("analysis", "results")
figures_dir <- file.path(results_dir, "figures")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

required_functions <- c(
  "ponte_settings",
  "ponte_prepare_bmi_panel",
  "ponte_build_bmi_synthetic_cohorts",
  "ponte_fit_bmi_transition_hayes",
  "ponte_initialise_population",
  "ponte_validate_living_state",
  "ponte_validate_life_table",
  "ponte_validate_mortality_risks",
  "ponte_validate_entrant_donor_pool",
  "ponte_validate_entrant_targets",
  "ponte_run_simulation",
  "ponte_verify_simulation_output"
)

missing_functions <- required_functions[
  !vapply(
    required_functions,
    exists,
    logical(1),
    where = asNamespace("pontept"),
    inherits = FALSE
  )
]

if (length(missing_functions) > 0L) {
  stop(
    "Required functions are absent from the `pontept` package: ",
    paste(missing_functions, collapse = ", "),
    call. = FALSE
  )
}

write_csv_result <- function(data, name) {
  readr::write_csv(
    data,
    file.path(results_dir, name),
    na = ""
  )

  invisible(data)
}

save_rds_result <- function(object, name) {
  saveRDS(
    object,
    file.path(results_dir, name)
  )

  invisible(object)
}

source("analysis/run_full_analysis.R", echo = TRUE)




save_rds_result <- function(object, file_name) {
  saveRDS(object, file.path(results_dir, file_name))
  invisible(object)
}

write_csv_result <- function(data, file_name) {
  utils::write.csv(
    data,
    file = file.path(results_dir, file_name),
    row.names = FALSE,
    na = ""
  )
  invisible(data)
}

require_object <- function(name, envir = parent.frame()) {
  if (!exists(name, envir = envir, inherits = TRUE)) {
    stop(sprintf("Required object `%s` is not available.", name), call. = FALSE)
  }

  get(name, envir = envir, inherits = TRUE)
}

# -----------------------------------------------------------------------------
# Module 1: Entrant targets for historical validation
# -----------------------------------------------------------------------------
settings_historical <- require_object("settings_historical")
donor_pool_1998 <- require_object("donor_pool_1998")

baseline_entrant_targets <- donor_pool_1998 |>
  group_by(sex) |>
  summarise(
    entrant_population = sum(survey_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    sex = as.character(sex),
    entrant_population = as.numeric(entrant_population)
  )

entrant_targets_historical <- tidyr::expand_grid(
  entry_year = seq.int(
    as.integer(settings_historical$initial_year) + 1L,
    as.integer(settings_historical$projection_end_year)
  ),
  sex = c("men", "women")
) |>
  left_join(baseline_entrant_targets, by = "sex") |>
  mutate(
    entry_year = as.integer(entry_year),
    entrant_population = as.numeric(entrant_population)
  ) |>
  arrange(entry_year, sex)

if (anyNA(entrant_targets_historical$entrant_population)) {
  missing_sexes <- entrant_targets_historical |>
    filter(is.na(entrant_population)) |>
    distinct(sex) |>
    pull(sex)

  stop(
    "Historical entrant targets could not be constructed for: ",
    paste(missing_sexes, collapse = ", "),
    ". Check `donor_pool_1998$sex`.",
    call. = FALSE
  )
}

ponte_validate_entrant_targets(
  entrant_targets = entrant_targets_historical,
  start_year = as.integer(settings_historical$initial_year) + 1L,
  end_year = as.integer(settings_historical$projection_end_year)
)

save_rds_result(baseline_entrant_targets, "baseline_entrant_targets.rds")
save_rds_result(entrant_targets_historical, "entrant_targets_historical.rds")
write_csv_result(entrant_targets_historical, "entrant_targets_historical.csv")

# -----------------------------------------------------------------------------
# Historical simulation
#
# This block runs only when all required objects have been created upstream.
# It assumes your validated one-year transition is used internally by
# ponte_run_simulation().
# -----------------------------------------------------------------------------
historical_required <- c(
  "prepared_inputs",
  "rr_bmi",
  "rr_education",
  "hayes_bmi_model"
)

historical_missing <- historical_required[
  !vapply(historical_required, exists, logical(1), inherits = TRUE)
]

if (length(historical_missing) == 0L) {
  historical_population <- prepared_inputs$population_1998
  historical_life_table <- prepared_inputs$life_table

  if (is.null(historical_population) || is.null(historical_life_table)) {
    stop(
      "`prepared_inputs` must contain `population_1998` and `life_table`.",
      call. = FALSE
    )
  }

  historical_n_entrants <- as.integer(round(sum(baseline_entrant_targets$entrant_population)))

  simulation_historical <- ponte_run_simulation(
    population = historical_population,
    settings = settings_historical,
    life_table = historical_life_table,
    rr_bmi = rr_bmi,
    rr_education = rr_education,
    donor_pool = donor_pool_1998,
    entrant_targets = entrant_targets_historical,
    hayes_bmi_model = hayes_bmi_model,
    bmi_update_method = "historical",
    stochastic_bmi = TRUE,
    education_source = "existing"
  )

  save_rds_result(simulation_historical, "simulation_historical.rds")
  write_csv_result(simulation_historical, "simulation_historical.csv")

  historical_population_summary <- simulation_historical |>
    filter(alive) |>
    group_by(year, sex) |>
    summarise(
      population_n = n(),
      population_weight = if ("survey_weight" %in% names(pick(everything()))) {
        sum(survey_weight, na.rm = TRUE)
      } else {
        as.numeric(n())
      },
      mean_bmi = mean(bmi, na.rm = TRUE),
      obesity_prevalence = mean(bmi >= 30, na.rm = TRUE),
      .groups = "drop"
    )

  save_rds_result(historical_population_summary, "historical_population_summary.rds")
  write_csv_result(historical_population_summary, "historical_population_summary.csv")
} else {
  message(
    "Historical simulation skipped; missing objects: ",
    paste(historical_missing, collapse = ", ")
  )
}

# -----------------------------------------------------------------------------
# 2020-2040 projection
#
# Define settings_projection, population_2020, donor_pool_2020, and
# entrant_targets_projection upstream. If your donor pool is deliberately held
# at 1998, assign donor_pool_2020 <- donor_pool_1998 before running this script.
# -----------------------------------------------------------------------------
projection_required <- c(
  "settings_projection",
  "population_2020",
  "donor_pool_2020",
  "entrant_targets_projection",
  "life_table_projection",
  "rr_bmi",
  "rr_education",
  "hayes_bmi_model"
)

projection_missing <- projection_required[
  !vapply(projection_required, exists, logical(1), inherits = TRUE)
]

if (length(projection_missing) == 0L) {
  ponte_validate_entrant_targets(
    entrant_targets = entrant_targets_projection,
    start_year = as.integer(settings_projection$initial_year) + 1L,
    end_year = as.integer(settings_projection$projection_end_year)
  )

  simulation_projection <- ponte_run_simulation(
    population = population_2020,
    settings = settings_projection,
    life_table = life_table_projection,
    rr_bmi = rr_bmi,
    rr_education = rr_education,
    donor_pool = donor_pool_2020,
    entrant_targets = entrant_targets_projection,
    hayes_bmi_model = hayes_bmi_model,
    bmi_update_method = "historical",
    stochastic_bmi = TRUE,
    education_source = "existing"
  )

  save_rds_result(simulation_projection, "simulation_projection_2020_2040.rds")
  write_csv_result(simulation_projection, "simulation_projection_2020_2040.csv")

  projection_obesity_summary <- simulation_projection |>
    filter(alive) |>
    group_by(year, sex) |>
    summarise(
      population_n = n(),
      mean_bmi = mean(bmi, na.rm = TRUE),
      overweight_prevalence = mean(bmi >= 25 & bmi < 30, na.rm = TRUE),
      obesity_prevalence = mean(bmi >= 30, na.rm = TRUE),
      obesity_class_II_prevalence = mean(bmi >= 35, na.rm = TRUE),
      obesity_class_III_prevalence = mean(bmi >= 40, na.rm = TRUE),
      .groups = "drop"
    )

  save_rds_result(projection_obesity_summary, "projection_obesity_summary_2020_2040.rds")
  write_csv_result(projection_obesity_summary, "projection_obesity_summary_2020_2040.csv")
} else {
  message(
    "2020-2040 projection skipped; missing objects: ",
    paste(projection_missing, collapse = ", ")
  )
}

# -----------------------------------------------------------------------------
# Modules 2 and 3
#
# Deliberately do not calculate YLD/DALYs or costs unless the complete,
# validated disability-weight and cost specifications are supplied upstream.
# -----------------------------------------------------------------------------
if (!exists("disability_weight_specification", inherits = TRUE)) {
  message("Module 2 YLD/DALY calculations skipped: no disability-weight specification supplied.")
}

if (!exists("borges_cost_parameters", inherits = TRUE)) {
  message("Module 3 cost calculations skipped: no Borges cost parameter set supplied.")
}

message("Available analysis outputs were written to: ", normalizePath(results_dir))
