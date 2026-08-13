#' Build BMI synthetic-cohort transitions
#'
#' Matches records between survey years separated by five years. Records are
#' matched on sex, five-year age group, and education level. Each matched pair
#' produces an annualized BMI change observation.
#'
#' @param bmi_panel A prepared BMI panel containing `age_group`.
#'
#' @return A data frame of matched synthetic-cohort BMI transitions.
#' @export
#'
#' @examples
#' bmi_panel <- data.frame(
#'   year = c(2015L, 2015L, 2020L, 2020L),
#'   sex = c("men", "women", "men", "women"),
#'   age = c(20L, 20L, 20L, 20L),
#'   education_level2 = c("low_medium", "higher", "low_medium", "higher"),
#'   survey_weight = c(800, 900, 850, 950),
#'   bmi = c(22.1, 23.8, 25.4, 24.7)
#' )
#'
#' ponte_build_bmi_synthetic_cohorts(
#'   bmi_panel = ponte_prepare_bmi_panel(
#'     bmi_panel = bmi_panel
#'   )
#' )
ponte_build_bmi_synthetic_cohorts <- function(
    bmi_panel
) {
  ponte_validate_bmi_panel(
    bmi_panel = bmi_panel
  )

  if (!"age_group" %in% names(bmi_panel)) {
    stop(
      "`bmi_panel` must contain an `age_group` column. ",
      "Use `ponte_prepare_bmi_panel()` first.",
      call. = FALSE
    )
  }

  expected_age_group <- ponte_bmi_age_group(
    age = bmi_panel$age
  )

  if (
    any(
      as.character(bmi_panel$age_group) !=
      expected_age_group
    )
  ) {
    stop(
      "`bmi_panel$age_group` must match the five-year group implied by `age`.",
      call. = FALSE
    )
  }

  survey_years <- sort(
    unique(
      as.integer(bmi_panel$year)
    )
  )

  baseline_years <- survey_years[
    (survey_years + 5L) %in% survey_years
  ]

  if (length(baseline_years) == 0L) {
    stop(
      "`bmi_panel` must contain at least one pair of survey years separated by 5 years.",
      call. = FALSE
    )
  }

  build_one_transition <- function(
    baseline_year
  ) {
    follow_up_year <- as.integer(
      baseline_year + 5L
    )

    baseline_data <- bmi_panel[
      bmi_panel$year == baseline_year,
      c(
        "sex",
        "education_level2",
        "age_group",
        "age",
        "bmi",
        "survey_weight"
      ),
      drop = FALSE
    ]

    follow_up_data <- bmi_panel[
      bmi_panel$year == follow_up_year,
      c(
        "sex",
        "education_level2",
        "age_group",
        "age",
        "bmi",
        "survey_weight"
      ),
      drop = FALSE
    ]

    names(baseline_data) <- c(
      "sex",
      "education_level2",
      "age_group",
      "age_t",
      "bmi_t",
      "wt_t"
    )

    names(follow_up_data) <- c(
      "sex",
      "education_level2",
      "age_group",
      "age_t1",
      "bmi_t1",
      "wt_t1"
    )

    matched_data <- merge(
      x = baseline_data,
      y = follow_up_data,
      by = c(
        "sex",
        "education_level2",
        "age_group"
      ),
      all = FALSE
    )

    if (nrow(matched_data) == 0L) {
      return(
        NULL
      )
    }

    data.frame(
      year_t = rep(
        as.integer(baseline_year),
        nrow(matched_data)
      ),
      year_t1 = rep(
        follow_up_year,
        nrow(matched_data)
      ),
      sex = matched_data$sex,
      education_level2 = matched_data$education_level2,
      age_group = matched_data$age_group,
      age_t = matched_data$age_t,
      age_t1 = matched_data$age_t1,
      bmi_t = matched_data$bmi_t,
      bmi_t1 = matched_data$bmi_t1,
      annual_bmi_change = (
        matched_data$bmi_t1 -
          matched_data$bmi_t
      ) / 5,
      wt_t = matched_data$wt_t,
      wt_t1 = matched_data$wt_t1,
      wt_combined = matched_data$wt_t *
        matched_data$wt_t1,
      stringsAsFactors = FALSE
    )
  }

  transition_data <- lapply(
    baseline_years,
    build_one_transition
  )

  transition_data <- Filter(
    Negate(is.null),
    transition_data
  )

  if (length(transition_data) == 0L) {
    stop(
      "No synthetic-cohort matches were found across the available 5-year survey pairs.",
      call. = FALSE
    )
  }

  do.call(
    rbind,
    transition_data
  )
}
