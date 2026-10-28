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

## 2. Software paper (`simeq_analysis_clean.qmd`)

Change the life-table chunk to read the new file:

```r
life_table_all <- utils::read.csv(
  file.path(project_root, "data", "life_tables", "pt_life_table_model_input_1998_2040_lc.csv")
)
life_table <- life_table_all[, c("year", "age", "sex", "qx")]
ref_life_expectancy <- subset(life_table_all, year == 2019L)[, c("age", "sex", "ex")]
```

Then add `last_observed_year = 2024L` to every `simeq_run_simulation()` call. Without it, the
hybrid calibration would treat 2025–2040 as observed and recalibrate every year, so BMI change
would stop changing the number of deaths.

## 3. Budget paper (`fixed_budget_obesity_allocation_v30.qmd`)

Replace

```r
last_life_table_year <- max(as.integer(life_table$year))
if (last_life_table_year != 2024L) stop(...)
```

with

```r
last_life_table_year <- 2024L   # last observed table; 2025+ are Lee-Carter projections
```

The calibration stays fixed on the 2024 population as before. `simeq_select_life_table_year()`
now picks the projected table for each year from 2025.

## 4. Text to change after rerunning

- Methods and Table 1: "2024 thereafter" becomes "projected to 2040 with a Lee-Carter model
  fitted to 1998–2024 (excluding 2020–2021)".
- The qmd sentence "the 2024 table is carried forward for later projection years" needs the
  same change.
