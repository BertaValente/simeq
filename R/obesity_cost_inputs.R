#' Published direct medical costs related to overweight and obesity
#'
#' Returns aggregated 2018 direct medical costs from Borges et al. (2025).
#' Values are reported in thousands of 2018 euros and cover inpatient,
#' outpatient, and pharmacological treatment costs from the Portuguese
#' National Health Service perspective.
#'
#' @return A tibble with cost disease groups and costs in thousands of euros.
#' @references Borges M, et al. Burden of Disease and Cost of Illness of
#'   Overweight and Obesity in Portugal. Obesity Facts. 2025;18(2):107-120.
#'   doi:10.1159/000541781.
#' @export
obesity_direct_costs_2018 <- function() {
  tibble::tribble(
    ~disease_group,                  ~inpatient, ~outpatient, ~pharmacological, ~total,
    "obesity_overweight",            5468,       1447,        6368,             13283,
    "diabetes_mellitus_type_2",      7796,       38458,       341871,           388126,
    "ischaemic_heart_disease",       25805,      48719,       63312,            137836,
    "stroke",                        32780,      96493,       12891,            142164,
    "hypertensive_hd_af",            9085,       104147,      39627,            152860,
    "cancer_all",                    17852,      7198,        19585,            44635,
    "osteoarthritis",                6322,       6533,        8693,             21548,
    "gout",                          123,        2677,        186,              2985,
    "depression",                    477,        6823,        19542,            26843,
    "alzheimers_disease",            356,        31829,       4085,             36270,
    "chronic_kidney_disease",        8094,       125645,      0,                133739,
    "gallbladder_biliary_diseases",  9129,       3318,        0,                12447,
    "asthma",                        447,        10043,       16517,            27007,
    "cataracts",                     6197,       1348,        242,              7787
  )
}

#' Published overweight-and-obesity population-attributable fractions
#'
#' Returns disease-specific PAFs reported by Borges et al. (2025) for
#' combined overweight and obesity. PAFs are proportions, not percentages.
#'
#' @return A tibble containing disease-specific overweight-and-obesity PAFs.
#' @references Borges M, et al. Burden of Disease and Cost of Illness of
#'   Overweight and Obesity in Portugal. Obesity Facts. 2025;18(2):107-120.
#'   doi:10.1159/000541781.
#' @export
obesity_overweight_pafs <- function() {
  tibble::tribble(
    ~disease_group_raw,                   ~paf_ow_ob,
    "diabetes_mellitus_type_2",           0.794,
    "ischaemic_heart_disease",            0.358,
    "ischaemic_stroke",                   0.428,
    "haemorrhagic_stroke",                0.629,
    "atrial_fibrillation",                0.210,
    "cancer_oesophageal",                 0.221,
    "cancer_colorectal",                  0.071,
    "cancer_liver",                       0.140,
    "cancer_gallbladder_biliary",         0.168,
    "cancer_pancreatic",                  0.055,
    "cancer_breast",                      0.018,
    "cancer_uterine",                     0.375,
    "cancer_ovarian",                     0.028,
    "cancer_kidney",                      0.178,
    "cancer_thyroid",                     0.110,
    "cancer_multiple_myeloma",            0.060,
    "cancer_leukaemia",                   0.073,
    "osteoarthritis",                     0.152,
    "gout",                               0.314,
    "depression",                         0.199,
    "alzheimers_disease",                 0.168,
    "ckd_diabetes",                       0.387,
    "ckd_hypertension",                   0.391,
    "ckd_glomerulo",                      0.386,
    "ckd_other",                          0.384,
    "gallbladder_biliary_diseases",       0.235,
    "asthma",                             0.242,
    "cataracts",                          0.068
  ) |>
    dplyr::mutate(
      disease_group = dplyr::case_when(
        disease_group_raw == "diabetes_mellitus_type_2" ~
          "diabetes_mellitus_type_2",
        disease_group_raw == "ischaemic_heart_disease" ~
          "ischaemic_heart_disease",
        disease_group_raw %in% c(
          "ischaemic_stroke",
          "haemorrhagic_stroke"
        ) ~ "stroke",
        disease_group_raw == "atrial_fibrillation" ~
          "hypertensive_hd_af",
        stringr::str_starts(disease_group_raw, "cancer_") ~
          "cancer_all",
        disease_group_raw == "osteoarthritis" ~
          "osteoarthritis",
        disease_group_raw == "gout" ~
          "gout",
        disease_group_raw == "depression" ~
          "depression",
        disease_group_raw == "alzheimers_disease" ~
          "alzheimers_disease",
        stringr::str_starts(disease_group_raw, "ckd_") ~
          "chronic_kidney_disease",
        disease_group_raw == "gallbladder_biliary_diseases" ~
          "gallbladder_biliary_diseases",
        disease_group_raw == "asthma" ~
          "asthma",
        disease_group_raw == "cataracts" ~
          "cataracts",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(disease_group)) |>
    dplyr::group_by(disease_group) |>
    dplyr::summarise(
      paf_ow_ob = mean(paf_ow_ob),
      .groups = "drop"
    )
}
