#' Transform BMI relative risks to an obesity contrast
#'
#' Converts GBD relative risks reported per 5 kg/m2 increase in BMI into
#' relative risks for an obesity BMI value compared with a reference BMI value.
#' The transformed risks are averaged across age bands for each outcome and sex.
#'
#' @param rr_matrix A data frame returned by `read_gbd_bmi_rr()`.
#' @param bmi_reference Reference BMI value.
#' @param bmi_obesity BMI value representing obesity.
#'
#' @return A tibble containing one obesity-versus-reference relative risk per
#'   outcome and sex, with lower and upper uncertainty bounds when available.
#' @export
transform_bmi_relative_risks <- function(
    rr_matrix,
    bmi_reference = 22,
    bmi_obesity = 32
) {
  required_columns <- c(
    "disease",
    "sex",
    "age_band",
    "rr_per_5",
    "rr_low",
    "rr_high"
  )

  missing_columns <- setdiff(required_columns, names(rr_matrix))

  if (length(missing_columns) > 0L) {
    stop(
      "rr_matrix is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(bmi_reference) != 1L ||
      length(bmi_obesity) != 1L ||
      !is.finite(bmi_reference) ||
      !is.finite(bmi_obesity) ||
      bmi_obesity <= bmi_reference) {
    stop(
      "bmi_obesity must be one finite value greater than bmi_reference.",
      call. = FALSE
    )
  }

  scale_factor <- (bmi_obesity - bmi_reference) / 5

  transformed <- rr_matrix |>
    dplyr::mutate(
      rr_obesity_vs_normal = rr_per_5 ^ scale_factor,
      rr_obesity_vs_normal_lower = rr_low ^ scale_factor,
      rr_obesity_vs_normal_upper = rr_high ^ scale_factor
    )

  transformed |>
    dplyr::group_by(disease, sex) |>
    dplyr::summarise(
      rr_obesity_vs_normal = mean(
        rr_obesity_vs_normal,
        na.rm = TRUE
      ),
      rr_obesity_vs_normal_lower = mean(
        rr_obesity_vs_normal_lower,
        na.rm = TRUE
      ),
      rr_obesity_vs_normal_upper = mean(
        rr_obesity_vs_normal_upper,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(outcome = disease) |>
    dplyr::mutate(
      outcome_lc = tolower(outcome)
    )
}
