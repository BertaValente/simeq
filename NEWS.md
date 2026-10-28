# simeq (development version): SIMEQ-PT v10

- `simeq_run_simulation()` gains `last_observed_year`: hybrid mortality
  calibration rescales each year up to this year and then keeps the scaling,
  so a life table that also holds projected years (e.g. Lee-Carter) changes
  baseline mortality without switching annual calibration back on.
- New `simeq_combine_education_upgrade()` stacks education upgrade rates by
  calendar period.
- `calculate_annual_obesity_costs()`: default `discount_rate` is now 0.04;
  the simulation output is reduced to living person-years and the needed
  columns before costing.
- `analysis/simeq_pt_analysis_v10.qmd`: SIMEQ-PT v10 analysis (50
  replications, Lee-Carter mortality, BMI calibration revision 1). The
  `data-raw/` and `mortality_lc/` scripts rebuild its inputs; the National
  Health Survey microdata and HMD-derived life tables are not distributed.

# simeq (development version)

Changes made in response to the paper I reviewer comments and the function
review.

### Performance

- `calculate_obesity_pafs_individual()` computes the mean relative risk with
  one vectorised step per sex and age band instead of joining every person
  to every GBD outcome, and `calculate_annual_obesity_costs()` works on one
  year of living person-years at a time and skips the unused sex split in
  the per-replication runs. Results are unchanged; the 50-replication cost
  runs take minutes instead of hours.

### Changed results (defaults)

- Annual cycle order: mortality is now applied first, using BMI at the start
  of the year, and BMI is then updated for survivors (previously BMI was
  updated first). Deaths keep their start-of-year BMI.
- `simeq_run_simulation()`: `mortality_calibration` now defaults to
  `"hybrid"`. The life-table hazard is rescaled every year while an observed
  (year-specific) life table exists, so deaths follow the observed tables;
  after the last observed year the scaling is fixed on that year's
  population, so projected changes in BMI and education change the number
  of deaths. `"baseline"` fixes the scaling on the initial population, and
  `"annual"` gives the previous behaviour.
- People who pass the maximum age (84) now leave the modelled population
  instead of staying at 84 with the 84-year-old death risk.
- `simeq_calculate_qalys()`: people who die in a cycle now contribute half a
  year at their utility in their death year (`half_cycle_deaths = TRUE`),
  instead of zero. Mean annual QALY per person is now total QALYs divided by
  weighted person-years.
- `calculate_annual_obesity_costs()` uses one method: disease PAFs from each
  person's BMI (new `calculate_obesity_pafs_individual()`), rescaled against
  the simulated base-year (2018) high-BMI PAF instead of the published Borges
  PAFs, so 2018 reproduces the published costs and later years change only
  with the simulated BMI distribution. New `exposure = "obesity"` gives
  obesity-only costs. Direct treatment costs follow the exposure prevalence.
  Removed arguments: `reference_paf_by_group`, `bmi_overweight`,
  `bmi_obesity`. Removed `calculate_obesity_pafs_age_sex()` (representative
  category BMIs).
- `calculate_annual_obesity_costs()`: annual totals now include the direct
  overweight and obesity treatment costs at 100% attribution
  (`include_direct_costs = TRUE`), matching the paper and Table S8.
- `calculate_annual_obesity_costs()`: costs by sex now add up to the national
  total. Each disease group's national cost is allocated between sexes by PAF
  times weighted population.

### New options (defaults keep previous behaviour)

- `simeq_fit_bmi_transition_percentile()` and `simeq_update_bmi_percentile()`,
  used through `bmi_update_method = "percentile"` (with
  `bmi_percentile_model`) in `simeq_run_simulation()` and
  `simeq_advance_one_year()`.
  - Synthetic cohorts follow the same birth cohort between NHS waves, with
    ages shifted by the exact interval.
  - For each cohort, the annual change is estimated at the 10th to 90th
    BMI percentiles and regressed on the mean model's predictors.
  - Each simulated person gets the change for their rank within their sex,
    education and five-year age group, so the upper tail can rise faster
    than the median.
  - `bmi_period = "by_year"` uses each survey period's estimates for its own
    years.
  - `life_table`, `rr_bmi` and `rr_education` (all three or none) weight each
    earlier-wave respondent by their probability of surviving to the later
    wave under the simulation's own mortality model. The later wave holds
    survivors only, so without this the fitted change includes survivor
    selection that the simulation then applies again through BMI-specific
    mortality.
  - `sex_interactions = TRUE` lets the age, age-squared and survey-period
    terms differ by sex.

- `simeq_fit_education_upgrade()` and `simeq_update_education()`, used through
  the new `education_upgrade` argument of `simeq_run_simulation()` and
  `simeq_advance_one_year()`. Each year, after the BMI update, people aged
  20-34 with medium education move to high education with sex- and
  age-specific annual probabilities estimated from NHS synthetic cohorts.
  The default (`NULL`) keeps education fixed from entry.

- `simeq_calculate_qalys()`: `discount_rate` and `discount_base_year`, a
  discounted QALY column, and weighted QALYs lost to high BMI
  (`qaly_loss_weighted`), so QALYs and DALYs can be reported in the same unit.
  Two-level education (`lower`, `higher`) is accepted.
- `calculate_annual_obesity_costs(by_simulation = TRUE)`: per-replication
  totals and 95% Monte Carlo simulation intervals.
- `simeq_calculate_qalys(bmi_decrements = )`: override the Carrello et al.
  (2021) utility decrements, e.g. with their 95% CI bounds, for sensitivity
  analysis.
- New helper `simeq_mortality_calibration_rr()` for the fixed calibration.
- Education mortality risk can be given per year of schooling:
  `rr_education = data.frame(rr_per_year = 0.981)` applies
  `0.981 ^ education_years` (IHME-CHAIN 2024), optionally age-specific with
  `age_min` and `age_max`. The population and donor pools then need an
  `education_years` column, which entrants now keep.
- New `simeq_life_table_from_hmd()` builds the year-indexed `life_table`
  (and `ex` for `ref_life_expectancy`) from Human Mortality Database tables.
- Life tables may have a `year` column; the table at or before each year is
  used and the last one is carried forward (`simeq_select_life_table_year()`).
- `simeq_run_simulation(bmi_period = "by_year")`: predicts BMI change before
  the last survey with the matching survey period, instead of the latest one.
- `simeq_calculate_yll()` and `simeq_calculate_obesity_burden()`:
  `ref_life_expectancy` uses remaining life expectancy at the age of death.

### Bug fixes

- `simeq_calculate_obesity_burden()`: obesity-attributable YLL for deaths
  recorded in year t now uses the PAF of the year t-1 population, whose BMI
  determined those deaths (previously the survivors' PAF in year t).
- `simeq_format_qaly_table()` and `simeq_plot_qaly_trajectory()` now use the
  column names returned by `simeq_calculate_qalys()`.
- `calculate_annual_obesity_costs()` no longer needs `purrr` and is exported.
- `extract_simulated_obesity_prevalence_by_age_sex()` default weight column
  is `survey_weight` (was `surveyweight`).
- `simeq_update_bmi_hayes()` updates `bmi_category` and `obesity`.
- `simeq_update_bmi_historical()` uses regression predictions only after the
  last survey wave, as documented, and keeps `weight` in sync with BMI.
- `simeq_update_bmi()` accepts height in metres or centimetres when updating
  weight.
- Roxygen title of `simeq_generate_synthetic_population()` restored.

# simeq 0.1.0

## First public release

**simeq** is an individual-level dynamic microsimulation model for projecting
BMI trajectories, health burden, and healthcare costs among Portuguese adults
(ages 20–84) over the period 1998–2040.

### Core functionality

- **Module 1 — BMI trajectories**: Open-cohort microsimulation with annual BMI
  transitions using empirical transition matrices derived from the Portuguese
  National Health Survey (1998, 2005, 2014, 2019). Two BMI update methods
  are supported: `"legacy"` (multinomial logit) and `"historical"` (Hayes
  empirical approach).

- **Module 2 — Health burden**: Calculation of years of life lost (YLL),
  quality-adjusted life years (QALY), and disability-adjusted life years
  (DALY) attributable to overweight and obesity, using GBD 2019 BMI relative
  risks.

- **Module 3 — Economic burden**: Projection of obesity-attributable healthcare
  costs using Portuguese cost-of-illness estimates, with support for
  budget-allocation and proportionate universalism policy scenarios.

### Bundled data

- `simeq_synthetic_pop`: A synthetic baseline population (n = 2 000) for
  vignette examples and testing — no restricted data required.
- `inst/extdata/gbd_rr_2019.xlsx`: GBD 2019 BMI mortality relative risks.
- `inst/extdata/prob_dying_qx.xlsx`: Portuguese life tables.
- `inst/extdata/demografia_ine_98_2025.xls`: INE demographic validation data.
- `inst/extdata/eurostat_clean.xlsx`: Eurostat demographic validation data.

### Validation and testing

- 33 unit-test files covering all core modules.
- Comprehensive input validation (life tables, populations, transition models,
  entrant donor pools).
