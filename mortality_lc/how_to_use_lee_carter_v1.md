# Using the Lee-Carter life tables

## 1. Make the projected table (done once)

Run `mortality_lc/project_life_table_lee_carter_v1.R` from the project folder. It writes
`data/life_tables/pt_life_table_model_input_1998_2040_lc.csv`, which holds the observed
1998–2024 tables plus projected 2025–2040, with a `projected` column.

What it does:

- It fits Lee-Carter separately for each sex, ages 20–84, on 1998–2024. It leaves out 2020–2021 (COVID).
- It projects k(t) as a random walk with drift.
- It starts from the observed 2024 rates, so 2025 follows on smoothly from what the model uses now.

The first component explains 89% of the variation for men and 75% for women.

Projected decline in qx, 2024 to 2040:

| Age | Men | Women |
|---|---|---|
| 50 | about 1.5% a year | about 1.5% a year |
| 75 | 2.2% a year | 2.9% a year |
| 84 | 1.7% a year | 2.0% a year |

Temporary life expectancy from age 20 to 85:

| | 2024 | 2040 |
|---|---|---|
| Men | 57.4 years | 59.2 years |
| Women | 61.1 years | 62.2 years |

## 2. Using the table in a simulation

`analysis/simeq_pt_analysis_v10.qmd` reads the projected table:

```r
life_table_all <- utils::read.csv(
  file.path(project_root, "data", "life_tables", "pt_life_table_model_input_1998_2040_lc.csv")
)
life_table <- life_table_all[, c("year", "age", "sex", "qx")]
ref_life_expectancy <- subset(life_table_all, year == 2019L)[, c("age", "sex", "ex")]
```

Every `simeq_run_simulation()` call sets `last_observed_year = 2024L`. Without it, the hybrid
calibration would treat 2025–2040 as observed and recalibrate every year, so BMI change would
stop changing the number of deaths. `simeq_select_life_table_year()` picks the projected table
for each year from 2025.

## 3. Input data

The observed 1998–2024 tables (`data/life_tables/pt_life_table_model_input_1998_2024.csv`) are
built by `data-raw/prepare_life_tables.R` from Human Mortality Database period life tables. HMD
terms do not allow redistribution, so `data/life_tables/` is not in the repository; download the
Portuguese tables from <https://www.mortality.org> and run that script first.
