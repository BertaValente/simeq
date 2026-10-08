# SIMEQ-PT analysis workflow

`simeq_analysis.qmd` is the Quarto workflow that produces every table and figure of the SIMEQ software paper and its supplement. It uses the Portuguese reference implementation, SIMEQ-PT.

## Running it

1. Clone the repository and open `simeq.Rproj`.
2. Place the input files listed below in the paths shown, relative to the repository root.
3. Render from the repository root:

   ```sh
   quarto render analysis/simeq_analysis.qmd
   ```

The document loads the package with `devtools::load_all()`. The production run uses 50 replications and seed `20260817`. To iterate quickly with 5 replications, set `dev_mode <- TRUE` in the setup chunk. The full run produces about 52.8 million person-year records and needs roughly 6 GB of RAM.

Outputs go to `results/`, which is git-ignored: simulation `.rds` files, CSV tables, figures and Word tables. Simulation and cost results are cached there. Delete the cache files if the inputs or the model change.

## Inputs

None of the input files are in this repository.

| File | Used for | Access |
|---|---|---|
| `data-raw/model_inputs_prepared_reporting_bias_calibrated.rds` | Base case: 1998 population, BMI panel, donor pools (calibrated BMI) | Restricted. Derived from Portuguese National Health Survey (NHS) microdata |
| `data-raw/model_inputs_prepared_base_case.rds` | Self-reported BMI sensitivity analysis | Restricted (NHS microdata) |
| `data-raw/nhs_2025_validation.rds` | Out-of-sample validation against NHS 2025 | Restricted (NHS microdata) |
| `data-raw/GBD_RR_2019/gbd_rr_2019.xlsx` | Module 3: GBD 2019 BMI relative risks | Public: GBD 2019 risk-factor relative risks (IHME) |
| `data-raw/cpi_portugal_hicp_2018base.csv` | Module 3: price adjustment (columns `year`, `cpi_index`, 2018 = 100) | Public: Eurostat `PRC_HICP_AIND`, Portugal, all items |
| `data/life_tables/pt_life_table_model_input_1998_2024.csv` | Mortality and YLL (columns `year`, `age`, `sex`, `qx`, `ex`; ages 20–84) | Public: Human Mortality Database, Portugal 1998–2024 |

NHS microdata are owned by Statistics Portugal (INE) and the National Health Institute Doutor Ricardo Jorge (INSA). Researchers can request them from the data owners. The model-input files are built from these microdata by the data-preparation scripts.

Other inputs ship with the package: the Borges et al. 2018 cost matrix, the cost–outcome mapping, and the EQ-5D-5L norms and utility decrements.

## Settings used in the paper

- Simulation 1998–2040, ages 20–84, 50 replications, seed `20260817`
- BMI transitions: percentile synthetic cohorts with survivor weighting, calibrated BMI
- Costs: constant 2018 euros; 2026 present values discounted at 4% per year from 2026
