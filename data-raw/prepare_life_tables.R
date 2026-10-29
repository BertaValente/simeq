# Builds the year-specific life tables read by the SIMEQ-PT analysis:
#   data/life_tables/pt_life_table_model_input_1998_2024.csv (year, age, sex, qx, ex)
# from Portuguese period life tables by single year of age, one file per sex,
# in Human Mortality Database layout (columns Year, Age, qx, ex, ...):
#   data/life_tables/pt_life_tables_1998_2024_men.csv
#   data/life_tables/pt_life_tables_1998_2024_women.csv
# These source tables are not redistributed. Download them (HMD: Portugal,
# period life tables 1x1, mltper_1x1 and fltper_1x1; .txt files can be passed
# directly) and save them under the names above, then run this script from the
# project root. Next, run mortality_lc/project_life_table_lee_carter_v1.R to
# add the 2025-2040 projection.
devtools::load_all(here::here(), quiet = TRUE)

life_table_dir <- here::here("data", "life_tables")

life_table <- simeq_life_table_from_hmd(
  tables = list(
    men   = file.path(life_table_dir, "pt_life_tables_1998_2024_men.csv"),
    women = file.path(life_table_dir, "pt_life_tables_1998_2024_women.csv")
  ),
  min_age = 20L,
  max_age = 84L,
  years = 1998:2024
)

utils::write.csv(
  life_table,
  file.path(life_table_dir, "pt_life_table_model_input_1998_2024.csv"),
  row.names = FALSE
)
