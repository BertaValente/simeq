#' Assign five-year BMI-model age groups
#'
#' Assigns ages from 20 through 84 to the five-year age groups used in the BMI
#' synthetic-cohort workflow.
#'
#' @param age Numeric vector of integer ages from 20 through 84.
#'
#' @return A character vector of five-year age-group labels.
#' @export
#'
#' @examples
#' simeq_bmi_age_group(
#'   c(
#'     20L,
#'     24L,
#'     25L,
#'     84L
#'   )
#' )
simeq_bmi_age_group <- function(
    age
) {
  if (
    !is.numeric(age) ||
    any(is.na(age)) ||
    any(!is.finite(age)) ||
    any(age != as.integer(age)) ||
    any(age < 20L) ||
    any(age > 84L)
  ) {
    stop(
      "`age` must contain only integer values from 20 to 84.",
      call. = FALSE
    )
  }

  lower_age <- (
    (as.integer(age) - 20L) %/% 5L
  ) * 5L + 20L

  upper_age <- lower_age + 4L

  paste0(
    lower_age,
    "_",
    upper_age
  )
}
