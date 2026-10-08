#' Fit a Hayes-style annual BMI-transition model
#'
#' Estimates survey-weighted annual BMI changes from repeated cross-sectional
#' surveys by sex, five-year age group, and baseline BMI category. It then fits
#' an individual-level regression for annual BMI change using sex, age, current
#' BMI, and education level.
#'
#' @param bmi_panel A prepared BMI panel produced by
#'   `simeq_prepare_bmi_panel()`.
#' @param stable_tolerance Numeric threshold used to label annual BMI changes as
#'   decrease, stable, or increase.
#'
#' @return An object of class `simeq_bmi_transition_hayes`.
#' @export
simeq_fit_bmi_transition_hayes <- function(
    bmi_panel,
    stable_tolerance = 0.01
) {
  simeq_validate_bmi_panel(
    bmi_panel = bmi_panel
  )

  if (!"age_group" %in% names(bmi_panel)) {
    stop(
      "`bmi_panel` must contain `age_group`. ",
      "Use `simeq_prepare_bmi_panel()` first.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(stable_tolerance) ||
    length(stable_tolerance) != 1L ||
    is.na(stable_tolerance) ||
    !is.finite(stable_tolerance) ||
    stable_tolerance < 0
  ) {
    stop(
      "`stable_tolerance` must be one finite non-negative number.",
      call. = FALSE
    )
  }

  bmi_category <- simeq_bmi_category(
    bmi = bmi_panel$bmi
  )

  stratum_year <- data.frame(
    year = as.integer(bmi_panel$year),
    sex = as.character(bmi_panel$sex),
    age_group = as.character(bmi_panel$age_group),
    bmi_category = as.character(bmi_category),
    bmi = as.numeric(bmi_panel$bmi),
    survey_weight = as.numeric(bmi_panel$survey_weight),
    stringsAsFactors = FALSE
  )

  split_id <- interaction(
    stratum_year$year,
    stratum_year$sex,
    stratum_year$age_group,
    stratum_year$bmi_category,
    drop = TRUE,
    lex.order = TRUE
  )

  strata <- split(
    stratum_year,
    split_id
  )

  stratum_summary <- do.call(
    rbind,
    lapply(
      strata,
      function(x) {
        data.frame(
          year = as.integer(x$year[1L]),
          sex = as.character(x$sex[1L]),
          age_group = as.character(x$age_group[1L]),
          bmi_category = as.character(x$bmi_category[1L]),
          mean_bmi = stats::weighted.mean(
            x$bmi,
            w = x$survey_weight
          ),
          sample_n = nrow(x),
          survey_weight_total = sum(x$survey_weight),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  stratum_summary <- stratum_summary[
    order(
      stratum_summary$sex,
      stratum_summary$age_group,
      stratum_summary$bmi_category,
      stratum_summary$year
    ),
    ,
    drop = FALSE
  ]

  group_id <- interaction(
    stratum_summary$sex,
    stratum_summary$age_group,
    stratum_summary$bmi_category,
    drop = TRUE,
    lex.order = TRUE
  )

  groups <- split(
    stratum_summary,
    group_id
  )

  annual_changes <- do.call(
    rbind,
    lapply(
      groups,
      function(x) {
        x <- x[order(x$year), , drop = FALSE]

        if (nrow(x) < 2L) {
          return(NULL)
        }

        transition_rows <- data.frame(
          year_t = x$year[-nrow(x)],
          year_t1 = x$year[-1L],
          interval_years = x$year[-1L] - x$year[-nrow(x)],
          sex = x$sex[-nrow(x)],
          age_group = x$age_group[-nrow(x)],
          bmi_category = x$bmi_category[-nrow(x)],
          mean_bmi_t = x$mean_bmi[-nrow(x)],
          mean_bmi_t1 = x$mean_bmi[-1L],
          annual_bmi_change = (
            x$mean_bmi[-1L] -
              x$mean_bmi[-nrow(x)]
          ) / (
            x$year[-1L] -
              x$year[-nrow(x)]
          ),
          transition_weight = sqrt(
            x$survey_weight_total[-nrow(x)] *
              x$survey_weight_total[-1L]
          ),
          sample_n_t = x$sample_n[-nrow(x)],
          sample_n_t1 = x$sample_n[-1L],
          stringsAsFactors = FALSE
        )

        transition_rows[
          transition_rows$interval_years %in% c(5L, 7L, 9L),
          ,
          drop = FALSE
        ]
      }
    )
  )

  if (is.null(annual_changes) || nrow(annual_changes) == 0L) {
    stop(
      "No annual BMI changes could be constructed from `bmi_panel`.",
      call. = FALSE
    )
  }

  annual_changes$change_type <- ifelse(
    annual_changes$annual_bmi_change < -stable_tolerance,
    "decrease",
    ifelse(
      annual_changes$annual_bmi_change > stable_tolerance,
      "increase",
      "stable"
    )
  )

  individual_data <- data.frame(
    year_t = as.integer(bmi_panel$year),
    sex = as.character(bmi_panel$sex),
    age = as.numeric(bmi_panel$age),
    age_group = as.character(bmi_panel$age_group),
    bmi = as.numeric(bmi_panel$bmi),
    bmi_category = as.character(bmi_category),
    education_level2 = as.character(bmi_panel$education_level2),
    survey_weight = as.numeric(bmi_panel$survey_weight),
    stringsAsFactors = FALSE
  )

  regression_data <- merge(
    x = individual_data,
    y = annual_changes[
      ,
      c(
        "year_t",
        "sex",
        "age_group",
        "bmi_category",
        "annual_bmi_change",
        "transition_weight"
      ),
      drop = FALSE
    ],
    by = c(
      "year_t",
      "sex",
      "age_group",
      "bmi_category"
    ),
    all = FALSE
  )

  if (nrow(regression_data) == 0L) {
    stop(
      "No individual records could be linked to annual BMI changes.",
      call. = FALSE
    )
  }

  regression_weight <- regression_data$survey_weight *
    regression_data$transition_weight

  fitted_model <- stats::lm(
    annual_bmi_change ~
      sex +
      age +
      I(age ^ 2) +
      bmi +
      education_level2,
    data = regression_data,
    weights = regression_weight
  )

  residual_rmse <- sqrt(
    mean(
      stats::residuals(fitted_model) ^ 2
    )
  )

  structure(
    list(
      model = fitted_model,
      residual_rmse = as.numeric(residual_rmse),
      stratum_summary = stratum_summary,
      annual_changes = annual_changes,
      regression_data = regression_data,
      stable_tolerance = as.numeric(stable_tolerance)
    ),
    class = "simeq_bmi_transition_hayes"
  )
}
