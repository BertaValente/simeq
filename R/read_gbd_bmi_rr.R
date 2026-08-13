#' Read GBD BMI relative-risk matrix
#'
#' Imports and tidies a GBD relative-risk matrix containing relative risks
#' per 5 kg/m2 increase in body mass index, by disease, sex, and age band.
#'
#' @param path Path to the GBD Excel workbook.
#' @param sheet Excel sheet name or position.
#'
#' @return A tibble with disease, sex, age band, relative risk, and lower and
#'   upper uncertainty bounds.
#' @export
read_gbd_bmi_rr <- function(
    path = "GBD_RR_2019/gbd_rr_2019.xlsx",
    sheet = 1
) {
  if (!file.exists(path)) {
    stop(
      "GBD relative-risk workbook was not found at: ",
      path,
      call. = FALSE
    )
  }

  age_cols <- c(
    "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79",
    "80-84", "85-89", "90-94", "95+"
  )

  raw <- readxl::read_excel(
    path = path,
    sheet = sheet,
    col_names = TRUE
  )

  names(raw) <- names(raw) |>
    stringr::str_replace_all("\\n", " ") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_trim()

  required_columns <- c("disease", "sex", age_cols)

  missing_columns <- setdiff(required_columns, names(raw))

  if (length(missing_columns) > 0L) {
    stop(
      "The GBD workbook is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  rr_matrix <- raw |>
    dplyr::select(
      disease,
      sex,
      dplyr::all_of(age_cols)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(age_cols),
      names_to = "age_band",
      values_to = "rr_string"
    ) |>
    dplyr::filter(
      !is.na(rr_string),
      rr_string != "",
      rr_string != " "
    ) |>
    dplyr::mutate(
      disease = as.character(disease),
      sex = as.character(sex),
      rr_string = as.character(rr_string),
      rr_string = stringr::str_replace_all(rr_string, "\\r\\n", " "),
      rr_string = stringr::str_replace_all(rr_string, "\\n", " "),
      rr_string = stringr::str_replace_all(rr_string, "\u2013", "to"),
      rr_string = stringr::str_replace_all(rr_string, "\\s+", " "),
      rr_string = stringr::str_trim(rr_string),
      rr_per_5 = as.numeric(
        stringr::str_extract(rr_string, "^[0-9.]+")
      ),
      rr_low = as.numeric(
        stringr::str_match(
          rr_string,
          "\\(([0-9.]+)\\s*to\\s*([0-9.]+)\\)"
        )[, 2]
      ),
      rr_high = as.numeric(
        stringr::str_match(
          rr_string,
          "\\(([0-9.]+)\\s*to\\s*([0-9.]+)\\)"
        )[, 3]
      )
    ) |>
    dplyr::arrange(disease, sex, age_band)

  if (nrow(rr_matrix) == 0L) {
    stop(
      "No relative-risk records were read from the GBD workbook.",
      call. = FALSE
    )
  }

  if (any(!is.finite(rr_matrix$rr_per_5)) ||
      any(rr_matrix$rr_per_5 <= 0)) {
    stop(
      "The GBD workbook contains invalid relative-risk point estimates.",
      call. = FALSE
    )
  }

  rr_matrix
}
