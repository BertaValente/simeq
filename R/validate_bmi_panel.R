#' Validate an observed BMI panel
#'
#' Validates the repeated cross-sectional survey data used to construct
#' synthetic cohorts and fit annual BMI transitions.
#'
#' @param bmi_panel A data frame containing observed BMI survey records.
#'
#' @return Invisibly returns `TRUE` when validation succeeds.
#' @export
#'
#' @examples
#' bmi_panel <- data.frame(
#'   year = c(2015L, 2015L, 2020L, 2020L),
#'   sex = c("men", "women", "men", "women"),
#'   age = c(20L, 20L, 25L, 25L),
#'   education_level2 = c("low_medium", "higher", "low_medium", "higher"),
#'   survey_weight = c(800, 900, 850, 950),
#'   bmi = c(22.1, 23.8, 25.4, 24.7)
#' )
#'
#' simeq_validate_bmi_panel(
#'   bmi_panel = bmi_panel
#' )
simeq_validate_bmi_panel <- function(
    bmi_panel
) {
  if (!is.data.frame(bmi_panel)) {
    stop(
      "`bmi_panel` must be a data frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "year",
    "sex",
    "age",
    "education_level2",
    "survey_weight",
    "bmi"
  )

  missing_columns <- setdiff(
    required_columns,
    names(bmi_panel)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`bmi_panel` is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  if (nrow(bmi_panel) == 0L) {
    stop(
      "`bmi_panel` must contain at least one record.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(bmi_panel$year) ||
    any(is.na(bmi_panel$year)) ||
    any(!is.finite(bmi_panel$year)) ||
    any(bmi_panel$year != as.integer(bmi_panel$year))
  ) {
    stop(
      "`bmi_panel$year` must contain only finite integer values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(bmi_panel$age) ||
    any(is.na(bmi_panel$age)) ||
    any(!is.finite(bmi_panel$age)) ||
    any(bmi_panel$age != as.integer(bmi_panel$age)) ||
    any(bmi_panel$age < 20L) ||
    any(bmi_panel$age > 84L)
  ) {
    stop(
      "`bmi_panel$age` must contain only integer values from 20 to 84.",
      call. = FALSE
    )
  }

  sex_values <- as.character(
    bmi_panel$sex
  )

  if (
    any(is.na(sex_values)) ||
    any(!sex_values %in% c("men", "women"))
  ) {
    stop(
      "`bmi_panel$sex` must contain only `men` and `women`.",
      call. = FALSE
    )
  }

  if (
    !all(
      c("men", "women") %in% sex_values
    )
  ) {
    stop(
      "`bmi_panel$sex` must contain both `men` and `women`.",
      call. = FALSE
    )
  }

  education_values <- as.character(
    bmi_panel$education_level2
  )

  if (
    any(is.na(education_values)) ||
    any(!nzchar(education_values))
  ) {
    stop(
      "`bmi_panel$education_level2` must contain no missing or empty values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(bmi_panel$survey_weight) ||
    any(is.na(bmi_panel$survey_weight)) ||
    any(!is.finite(bmi_panel$survey_weight)) ||
    any(bmi_panel$survey_weight <= 0)
  ) {
    stop(
      "`bmi_panel$survey_weight` must contain only positive finite values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(bmi_panel$bmi) ||
    any(is.na(bmi_panel$bmi)) ||
    any(!is.finite(bmi_panel$bmi)) ||
    any(bmi_panel$bmi <= 0)
  ) {
    stop(
      "`bmi_panel$bmi` must contain only positive finite values.",
      call. = FALSE
    )
  }

  invisible(
    TRUE
  )
}

