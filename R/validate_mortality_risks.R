#' Validate PONTE-PT-O mortality relative-risk tables
#'
#' Validates the BMI-category and education relative-risk tables used to
#' adjust life-table annual death probabilities in the PONTE-PT-O model.
#'
#' @param rr_bmi A data frame with columns `sex`, `bmi_category`, and
#'   `rr_bmi`.
#' @param rr_education Either a data frame with columns `education_level2`
#'   and `rr_education` (one relative risk per education group), or a data
#'   frame with column `rr_per_year` (relative risk per additional year of
#'   schooling, applied to the state's `education_years`), optionally with
#'   `age_min` and `age_max` for age-specific values covering ages without
#'   overlap.
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
#' simeq_validate_mortality_risks(
#'   rr_bmi = rr_bmi,
#'   rr_education = rr_education
#' )
simeq_validate_mortality_risks <- function(
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

  per_year_education <- "rr_per_year" %in% names(rr_education)

  required_rr_education_columns <- if (per_year_education) {
    "rr_per_year"
  } else {
    c(
      "education_level2",
      "rr_education"
    )
  }

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

  if (per_year_education) {
    .validate_rr_education_per_year(rr_education)

    return(
      invisible(
        list(
          rr_bmi = rr_bmi,
          rr_education = rr_education
        )
      )
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

# Checks for the per-year-of-schooling education relative-risk format.
.validate_rr_education_per_year <- function(rr_education) {
  rr_per_year <- rr_education$rr_per_year

  if (
    !is.numeric(rr_per_year) ||
    anyNA(rr_per_year) ||
    any(!is.finite(rr_per_year)) ||
    any(rr_per_year <= 0)
  ) {
    stop(
      "`rr_education$rr_per_year` must contain positive, finite values.",
      call. = FALSE
    )
  }

  has_age_range <- all(c("age_min", "age_max") %in% names(rr_education))

  if (!has_age_range && nrow(rr_education) != 1L) {
    stop(
      "`rr_education` needs `age_min` and `age_max` when it has more than ",
      "one `rr_per_year` row.",
      call. = FALSE
    )
  }

  if (has_age_range) {
    ordered <- rr_education[order(rr_education$age_min), , drop = FALSE]

    if (
      anyNA(ordered$age_min) ||
      anyNA(ordered$age_max) ||
      any(ordered$age_min > ordered$age_max) ||
      any(utils::head(ordered$age_max, -1L) >= utils::tail(ordered$age_min, -1L))
    ) {
      stop(
        "`rr_education` age ranges must be increasing and must not overlap.",
        call. = FALSE
      )
    }
  }

  invisible(rr_education)
}
