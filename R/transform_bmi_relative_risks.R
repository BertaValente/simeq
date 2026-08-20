#' Transform BMI relative risks to exposure-versus-reference contrasts
#'
#' Converts GBD relative risks (reported per 5 kg/m² increase in BMI) into
#' relative risks at representative overweight and/or obesity BMI values
#' compared with a reference BMI. Relative risks are averaged across age bands
#' for each disease outcome and sex.
#'
#' @param rr_matrix A data frame returned by `read_gbd_bmi_rr()`.
#' @param bmi_reference Reference (normal-weight) BMI value. Default 22 kg/m².
#' @param bmi_obesity Representative obesity BMI value. Default 32 kg/m².
#' @param bmi_overweight Optional representative overweight BMI value
#'   (e.g. 27.5 kg/m²). When supplied, overweight-versus-reference relative
#'   risks are returned as additional columns `rr_overweight_vs_normal`,
#'   `rr_overweight_vs_normal_lower`, and `rr_overweight_vs_normal_upper`.
#'
#' @return A tibble with one row per disease outcome and sex. Columns always
#'   include the obesity-versus-reference relative risks. Overweight-versus-
#'   reference columns are added when `bmi_overweight` is not `NULL`.
#' @export
transform_bmi_relative_risks <- function(
    rr_matrix,
    bmi_reference  = 22,
    bmi_obesity    = 32,
    bmi_overweight = NULL
) {
  required_columns <- c("disease", "sex", "age_band",
                        "rr_per_5", "rr_low", "rr_high")

  missing_columns <- setdiff(required_columns, names(rr_matrix))
  if (length(missing_columns) > 0L) {
    stop(
      "rr_matrix is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(bmi_reference) != 1L || !is.finite(bmi_reference)) {
    stop("bmi_reference must be one finite value.", call. = FALSE)
  }

  if (length(bmi_obesity) != 1L || !is.finite(bmi_obesity) ||
      bmi_obesity <= bmi_reference) {
    stop(
      "bmi_obesity must be one finite value greater than bmi_reference.",
      call. = FALSE
    )
  }

  if (!is.null(bmi_overweight)) {
    if (length(bmi_overweight) != 1L || !is.finite(bmi_overweight) ||
        bmi_overweight <= bmi_reference || bmi_overweight >= bmi_obesity) {
      stop(
        "bmi_overweight must be one finite value between bmi_reference ",
        "and bmi_obesity.",
        call. = FALSE
      )
    }
  }

  scale_ob <- (bmi_obesity - bmi_reference) / 5

  transformed <- rr_matrix |>
    dplyr::mutate(
      rr_obesity_vs_normal       = rr_per_5 ^ scale_ob,
      rr_obesity_vs_normal_lower = rr_low   ^ scale_ob,
      rr_obesity_vs_normal_upper = rr_high  ^ scale_ob
    )

  if (!is.null(bmi_overweight)) {
    scale_ow <- (bmi_overweight - bmi_reference) / 5
    transformed <- transformed |>
      dplyr::mutate(
        rr_overweight_vs_normal       = rr_per_5 ^ scale_ow,
        rr_overweight_vs_normal_lower = rr_low   ^ scale_ow,
        rr_overweight_vs_normal_upper = rr_high  ^ scale_ow
      )
  }

  summary_cols <- c(
    "rr_obesity_vs_normal", "rr_obesity_vs_normal_lower",
    "rr_obesity_vs_normal_upper",
    if (!is.null(bmi_overweight))
      c("rr_overweight_vs_normal", "rr_overweight_vs_normal_lower",
        "rr_overweight_vs_normal_upper")
  )

  transformed |>
    dplyr::group_by(disease, sex) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(summary_cols), \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::rename(outcome = disease) |>
    dplyr::mutate(outcome_lc = tolower(outcome))
}
