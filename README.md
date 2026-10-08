# pontept <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R CMD Check](https://github.com/BertaValente/pontept/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BertaValente/pontept/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

**pontept** is an individual-level, stochastic, open-cohort microsimulation model for projecting obesity trajectories, health burden, and healthcare costs in the Portuguese adult population (ages 20–84) over the period 1998–2040.

The model comprises three modules:

| Module | Description |
|--------|-------------|
| **1 — BMI trajectories** | Simulates annual BMI transitions using empirical transition matrices derived from the Portuguese National Health Survey (1998, 2005, 2014, 2019), combined with demographic aging, mortality, and new entrants |
| **2 — Health burden** | Estimates years of life lost (YLL), quality-adjusted life years (QALY), and disability-adjusted life years (DALY) attributable to overweight and obesity |
| **3 — Economic burden** | Projects obesity-attributable healthcare costs and supports policy scenario analysis including budget-allocation experiments |

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("BertaValente/pontept")
```

## Data requirements

The full analysis uses the **Portuguese National Health Survey** (Inquérito Nacional de Saúde), which contains individual-level health and anthropometric data and is not publicly available. Researchers wishing to replicate the main analysis must request access through [INSA (Instituto Nacional de Saúde Doutor Ricardo Jorge)](https://www.insa.min-saude.pt/).

The following **reference datasets** are bundled with the package in `inst/extdata/` and do not require separate download:

- **GBD 2019 BMI relative risks** (`gbd_rr_2019.xlsx`) — from the [Global Burden of Disease study](https://vizhub.healthdata.org/gbd-results/)
- **Portuguese life tables** (`prob_dying_qx.xlsx`) — age- and sex-specific probabilities of death
- **Demographic validation data** (`demografia_ine_98_2025.xls`, `eurostat_clean.xlsx`) — from INE and Eurostat

## Quick start

The example below uses the bundled reference data and a small synthetic population to demonstrate the simulation workflow. See `vignettes/getting-started.Rmd` for a full walkthrough.

```r
library(pontept)
library(dplyr)
library(readxl)

# 1. Load bundled reference data
life_table_path <- system.file("extdata", "prob_dying_qx.xlsx", package = "pontept")
gbd_rr_path     <- system.file("extdata", "gbd_rr_2019.xlsx",  package = "pontept")

life_table <- readxl::read_excel(life_table_path) |>
  ponte_validate_life_table()

rr_bmi <- read_gbd_bmi_rr(path = gbd_rr_path) |>
  transform_bmi_relative_risks()

# 2. Create simulation settings
settings <- ponte_settings(
  initial_year        = 2019,
  projection_end_year = 2030,
  n_simulations       = 10,
  seed                = 4321L
)

# 3. Build a synthetic baseline population (no NHS data required)
# In a real analysis, replace this with your prepared NHS-derived population.
population <- pontept_synthetic_pop   # small built-in dataset, n = 2 000

# 4. Run the simulation
# (requires a fitted BMI transition model; see vignette for full setup)
# sim_results <- ponte_run_simulation(
#   population    = population,
#   settings      = settings,
#   life_table    = life_table,
#   rr_bmi        = rr_bmi,
#   ...
# )
```

## Repository structure

```
pontept/
├── R/                  # Package functions (57 exported)
├── man/                # Roxygen2-generated documentation
├── tests/testthat/     # Unit tests (33 test files)
├── inst/extdata/       # Bundled reference data (GBD RRs, life tables)
├── vignettes/          # Getting-started vignette
├── data-raw/           # Raw data preparation scripts (not on GitHub for NHS data)
├── analysis/           # Full analysis scripts and Quarto reports
└── DESCRIPTION
```

## Citation

If you use **pontept** in your research, please cite:

> Valente B (2026). *pontept: Portuguese Obesity Trajectory Microsimulation*.
> R package version 0.1.0. <https://github.com/BertaValente/pontept>

## License

MIT © 2026 Berta Valente
