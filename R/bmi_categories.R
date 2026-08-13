#' Classify BMI values
#'
#' Classifies adult body mass index (BMI) values into the six BMI categories
#' used by the PONTE-PT-O mortality and health-outcomes models.
#'
#' @param bmi Numeric vector of body mass index values in kg/m^2.
#'
#' @return A character vector of BMI categories.
#' @export
#'
#' @examples
#' ponte_bmi_category(
#'   c(17.5, 22.0, 27.5, 32.0, 37.0, 42.0)
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

  cut(
    bmi,
    breaks = c(
      -Inf,
      18.5,
      25,
      30,
      35,
      40,
      Inf
    ),
    right = FALSE,
    labels = c(
      "underweight",
      "normal_weight",
      "overweight",
      "obesity_I",
      "obesity_II",
      "obesity_III"
    )
  ) |>
    as.character()
}
