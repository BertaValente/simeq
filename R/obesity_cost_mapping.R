#' Map GBD outcomes to obesity cost disease groups
#'
#' Returns the mapping used to aggregate GBD BMI relative-risk outcomes to the
#' disease groups reported in the Portuguese cost-of-illness study.
#'
#' @return A tibble with GBD outcome names and cost disease groups.
#' @export
obesity_cost_outcome_mapping <- function() {
  tibble::tribble(
    ~outcome_pattern,                            ~disease_group,
    "Diabetes mellitus type 2",                  "diabetes_mellitus_type_2",
    "Ischaemic heart disease",                   "ischaemic_heart_disease",
    "Ischaemic stroke",                          "stroke",
    "Intracerebral hemorrhage",                  "stroke",
    "Subarachnoid hemorrhage",                   "stroke",
    "Hypertensive heart disease",                "hypertensive_hd_af",
    "Atrial fibrillation and flutter",           "hypertensive_hd_af",
    "Oesophageal cancer",                        "cancer_all",
    "Colon and rectum cancer",                   "cancer_all",
    "Liver cancer due to hepatitis B",           "cancer_all",
    "Liver cancer due to hepatitis C",           "cancer_all",
    "Liver cancer due to alcohol use",           "cancer_all",
    "Gallbladder and biliary tract cancer",      "cancer_all",
    "Pancreatic cancer",                         "cancer_all",
    "Breast cancer",                             "cancer_all",
    "Uterine cancer",                            "cancer_all",
    "Ovarian cancer",                            "cancer_all",
    "Kidney cancer",                             "cancer_all",
    "Thyroid cancer",                            "cancer_all",
    "Multiple myeloma",                          "cancer_all",
    "Acute lymphoid leukaemia",                  "cancer_all",
    "Chronic lymphoid leukaemia",                "cancer_all",
    "Acute myeloid leukaemia",                   "cancer_all",
    "Chronic myeloid leukaemia",                 "cancer_all",
    "Other leukaemia",                           "cancer_all",
    "Osteoarthritis hip",                        "osteoarthritis",
    "Osteoarthritis knee",                       "osteoarthritis",
    "Low back pain",                             "osteoarthritis",
    "Gout",                                      "gout",
    "Depression",                                "depression",
    "Alzheimer's disease and other dementias",   "alzheimers_disease",
    "Chronic kidney disease due to diabetes mellitus type 2",
    "chronic_kidney_disease",
    "Chronic kidney disease due to hypertension",
    "chronic_kidney_disease",
    "Chronic kidney disease due to glomerulonephritis",
    "chronic_kidney_disease",
    "Chronic kidney disease due to other and unspecified causes",
    "chronic_kidney_disease",
    "Gallbladder and biliary diseases",
    "gallbladder_biliary_diseases",
    "Asthma",                                    "asthma",
    "Cataract",                                  "cataracts"
  )
}
