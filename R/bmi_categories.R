#' Classify BMI values
#'
#' Classifies body mass index (BMI) values using adult BMI categories.
#'
#' @param bmi Numeric vector of body mass index values in kg/m^2.
#'
#' @return A character vector of BMI categories.
#' @export
#'
#' @examples
#' ponte_bmi_category(
#'   c(17.5, 22.0, 27.5, 32.0)
#' )
ponte_bmi_category <- function(
    bmi
) {
  if (!is.numeric(bmi)) {
    stop(
      "`bmi` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.na(bmi) & !is.finite(bmi))) {
    stop(
      "`bmi` must not contain infinite values.",
      call. = FALSE
    )
  }

  category <- rep(
    NA_character_,
    length(bmi)
  )

  category[!is.na(bmi) & bmi < 18.5] <-
    "underweight"

  category[!is.na(bmi) & bmi >= 18.5 & bmi < 25] <-
    "normal_weight"

  category[!is.na(bmi) & bmi >= 25 & bmi < 30] <-
    "overweight"

  category[!is.na(bmi) & bmi >= 30] <-
    "obesity"

  category
}
