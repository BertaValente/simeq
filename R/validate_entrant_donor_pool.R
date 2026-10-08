#' Validate an entrant donor pool
#'
#' Validates historical age 20 to 24 records used as a donor pool for
#' generating new age-20 entrants in the open PONTE-PT-O cohort.
#'
#' @param donor_pool A data frame containing historical donor records.
#' @param entry_age Integer. Model entry age.
#'
#' @return The input `donor_pool`, invisibly, if all checks pass.
#' @export
#'
#' @examples
#' donor_pool <- data.frame(
#'   sex = c("men", "women"),
#'   age = c(21L, 23L),
#'   education_level = c("medium", "high"),
#'   bmi = c(24.1, 22.8),
#'   survey_weight = c(1000, 1200)
#' )
#'
#' simeq_validate_entrant_donor_pool(donor_pool)
simeq_validate_entrant_donor_pool <- function(
    donor_pool,
    entry_age = 20L
) {
  if (!is.data.frame(donor_pool)) {
    stop(
      "`donor_pool` must be a data frame.",
      call. = FALSE
    )
  }

  if (nrow(donor_pool) == 0L) {
    stop(
      "`donor_pool` must contain at least one record.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "sex",
    "age",
    "education_level",
    "bmi",
    "survey_weight"
  )

  missing_columns <- setdiff(
    required_columns,
    names(donor_pool)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`donor_pool` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  sex <- as.character(
    donor_pool$sex
  )

  if (
    anyNA(sex) ||
    any(
      !sex %in% c(
        "men",
        "women"
      )
    )
  ) {
    stop(
      "`donor_pool$sex` must contain only `men` and `women`.",
      call. = FALSE
    )
  }

  if (!all(c("men", "women") %in% sex)) {
    stop(
      "`donor_pool` must contain records for both `men` and `women`.",
      call. = FALSE
    )
  }

  age <- donor_pool$age

  if (
    !is.numeric(age) ||
    anyNA(age) ||
    any(!is.finite(age)) ||
    any(age != as.integer(age))
  ) {
    stop(
      "`donor_pool$age` must contain finite, non-missing integer values.",
      call. = FALSE
    )
  }

  maximum_donor_age <- as.integer(
    entry_age + 4L
  )

  if (any(age < entry_age | age > maximum_donor_age)) {
    stop(
      "`donor_pool` must contain only ages from ",
      entry_age,
      " to ",
      maximum_donor_age,
      ".",
      call. = FALSE
    )
  }

  education_level <- as.character(
    donor_pool$education_level
  )

  if (
    anyNA(education_level) ||
    any(
      !education_level %in% c(
        "low",
        "medium",
        "high"
      )
    )
  ) {
    stop(
      "`donor_pool$education_level` must contain only `low`, `medium`, ",
      "and `high`.",
      call. = FALSE
    )
  }

  bmi <- donor_pool$bmi

  if (
    !is.numeric(bmi) ||
    anyNA(bmi) ||
    any(!is.finite(bmi)) ||
    any(bmi <= 0)
  ) {
    stop(
      "`donor_pool$bmi` must contain positive, finite values.",
      call. = FALSE
    )
  }

  survey_weight <- donor_pool$survey_weight

  if (
    !is.numeric(survey_weight) ||
    anyNA(survey_weight) ||
    any(!is.finite(survey_weight)) ||
    any(survey_weight <= 0)
  ) {
    stop(
      "`donor_pool$survey_weight` must contain positive, finite values.",
      call. = FALSE
    )
  }

  invisible(donor_pool)
}
