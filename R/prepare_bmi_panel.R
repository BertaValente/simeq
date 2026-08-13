#' Prepare an observed BMI panel for synthetic-cohort matching
#'
#' Validates an observed BMI panel and adds the five-year age-group variable
#' used to match records between survey years.
#'
#' @param bmi_panel A data frame containing observed BMI survey records.
#'
#' @return The input data frame with an appended `age_group` column.
#' @export
#'
#' @examples
#' bmi_panel <- data.frame(
#'   year = c(2015L, 2015L, 2020L, 2020L),
#'   sex = c("men", "women", "men", "women"),
#'   age = c(20L, 24L, 25L, 84L),
#'   education_level2 = c("low_medium", "higher", "low_medium", "higher"),
#'   survey_weight = c(800, 900, 850, 950),
#'   bmi = c(22.1, 23.8, 25.4, 24.7)
#' )
#'
#' ponte_prepare_bmi_panel(
#'   bmi_panel = bmi_panel
#' )
ponte_prepare_bmi_panel <- function(
    bmi_panel
) {
  ponte_validate_bmi_panel(
    bmi_panel = bmi_panel
  )

  bmi_panel$age_group <- ponte_bmi_age_group(
    age = bmi_panel$age
  )

  bmi_panel
}

