#' Validate PONTE-PT-O mortality relative-risk tables
#'
#' Validates the BMI-category and education relative-risk tables used to
#' adjust life-table annual death probabilities in the PONTE-PT-O model.
#'
#' @param rr_bmi A data frame with columns `sex`, `bmi_category`, and
#'   `rr_bmi`.
#' @param rr_education A data frame with columns `education_level2` and
#'   `rr_education`.
#'
#' @return A list containing the validated `rr_bmi` and `rr_education`
#'   data frames, invisibly.
#' @export
#'
#' @examples
#' rr_bmi <- expand.grid(
#'   sex = c("men", "women"),
#'   bmi_category = c(
#'     "underweight",
#'     "normal_weight",
#'     "overweight",
#'     "obesity_I",
#'     "obesity_II",
#'     "obesity_III"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' rr_bmi$rr_bmi <- 1
#'
#' rr_education <- data.frame(
#'   education_level2 = c("lower", "higher"),
#'   rr_education = c(1, 1)
#' )
#'
#' ponte_validate_mortality_risks(
#'   rr_bmi = rr_bmi,
#'   rr_education = rr_education
#' )
ponte_validate_mortality_risks <- function(
    rr_bmi,
    rr_education
) {
  if (!is.data.frame(rr_bmi)) {
    stop(
      "`rr_bmi` must be a data frame.",
      call. = FALSE
    )
  }

  if (!is.data.frame(rr_education)) {
    stop(
      "`rr_education` must be a data frame.",
      call. = FALSE
    )
  }

  required_rr_bmi_columns <- c(
    "sex",
    "bmi_category",
    "rr_bmi"
  )

  missing_rr_bmi_columns <- setdiff(
    required_rr_bmi_columns,
    names(rr_bmi)
  )

  if (length(missing_rr_bmi_columns) > 0L) {
    stop(
      "`rr_bmi` is missing required columns: ",
      paste(missing_rr_bmi_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_rr_education_columns <- c(
    "education_level2",
    "rr_education"
  )

  missing_rr_education_columns <- setdiff(
    required_rr_education_columns,
    names(rr_education)
  )

  if (length(missing_rr_education_columns) > 0L) {
    stop(
      "`rr_education` is missing required columns: ",
      paste(missing_rr_education_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_sexes <- c(
    "men",
    "women"
  )

  required_bmi_categories <- c(
    "underweight",
    "normal_weight",
    "overweight",
    "obesity_I",
    "obesity_II",
    "obesity_III"
  )

  required_education_levels <- c(
    "lower",
    "higher"
  )

  sex <- as.character(
    rr_bmi$sex
  )

  if (
    anyNA(sex) ||
    any(!sex %in% required_sexes)
  ) {
    stop(
      "`rr_bmi$sex` must contain only `men` and `women`.",
      call. = FALSE
    )
  }

  bmi_category <- as.character(
    rr_bmi$bmi_category
  )

  if (
    anyNA(bmi_category) ||
    any(!bmi_category %in% required_bmi_categories)
  ) {
    stop(
      "`rr_bmi$bmi_category` contains unsupported BMI categories.",
      call. = FALSE
    )
  }

  rr_bmi_values <- rr_bmi$rr_bmi

  if (
    !is.numeric(rr_bmi_values) ||
    anyNA(rr_bmi_values) ||
    any(!is.finite(rr_bmi_values)) ||
    any(rr_bmi_values <= 0)
  ) {
    stop(
      "`rr_bmi$rr_bmi` must contain positive, finite values.",
      call. = FALSE
    )
  }

  rr_bmi_identifier <- paste(
    sex,
    bmi_category,
    sep = "::"
  )

  if (anyDuplicated(rr_bmi_identifier) > 0L) {
    stop(
      "`rr_bmi` must contain one record per sex-BMI-category combination.",
      call. = FALSE
    )
  }

  expected_rr_bmi <- expand.grid(
    sex = required_sexes,
    bmi_category = required_bmi_categories,
    stringsAsFactors = FALSE
  )

  expected_rr_bmi_identifier <- paste(
    expected_rr_bmi$sex,
    expected_rr_bmi$bmi_category,
    sep = "::"
  )

  if (
    any(
      !expected_rr_bmi_identifier %in%
      rr_bmi_identifier
    )
  ) {
    stop(
      "`rr_bmi` must cover every sex-BMI-category combination.",
      call. = FALSE
    )
  }

  education_level2 <- as.character(
    rr_education$education_level2
  )

  if (
    anyNA(education_level2) ||
    any(
      !education_level2 %in%
      required_education_levels
    )
  ) {
    stop(
      "`rr_education$education_level2` must contain only ",
      "`lower` and `higher`.",
      call. = FALSE
    )
  }

  rr_education_values <- rr_education$rr_education

  if (
    !is.numeric(rr_education_values) ||
    anyNA(rr_education_values) ||
    any(!is.finite(rr_education_values)) ||
    any(rr_education_values <= 0)
  ) {
    stop(
      "`rr_education$rr_education` must contain positive, finite values.",
      call. = FALSE
    )
  }

  if (anyDuplicated(education_level2) > 0L) {
    stop(
      "`rr_education` must contain one record per education level.",
      call. = FALSE
    )
  }

  if (
    any(
      !required_education_levels %in%
      education_level2
    )
  ) {
    stop(
      "`rr_education` must cover both `lower` and `higher` education.",
      call. = FALSE
    )
  }

  invisible(
    list(
      rr_bmi = rr_bmi,
      rr_education = rr_education
    )
  )
}
