#' Fit the annual BMI-transition model
#'
#' Fits an annual BMI-change model from synthetic-cohort transition records.
#' The fitted object contains the linear model, residual root mean squared
#' error, the projection-period value, and limits for annual BMI change.
#'
#' @param bmi_transitions A data frame created by
#'   `simeq_build_bmi_synthetic_cohorts()`.
#' @param projection_period Integer baseline survey year to use when predicting
#'   BMI change during the projection. Defaults to the latest available
#'   baseline survey year.
#' @param transition_limits Numeric vector of length two giving the lower and
#'   upper limits for simulated annual BMI change.
#'
#' @return An object of class `simeq_bmi_transition_model`.
#' @export
simeq_fit_bmi_transition <- function(
    bmi_transitions,
    projection_period = NULL,
    transition_limits = c(
      -2,
      2
    )
) {
  if (!is.data.frame(bmi_transitions)) {
    stop(
      "`bmi_transitions` must be a data frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "annual_bmi_change",
    "sex",
    "age_t",
    "bmi_t",
    "education_level2",
    "year_t",
    "transition_weight"
  )

  missing_columns <- setdiff(
    required_columns,
    names(bmi_transitions)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`bmi_transitions` is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  if (nrow(bmi_transitions) == 0L) {
    stop(
      "`bmi_transitions` must contain at least one record.",
      call. = FALSE
    )
  }

  numeric_columns <- c(
    "annual_bmi_change",
    "age_t",
    "bmi_t",
    "year_t"
  )

  for (column_name in numeric_columns) {
    values <- bmi_transitions[[
      column_name
    ]]

    if (
      !is.numeric(values) ||
      any(is.na(values)) ||
      any(!is.finite(values))
    ) {
      stop(
        "`bmi_transitions$",
        column_name,
        "` must contain only finite numeric values.",
        call. = FALSE
      )
    }
  }

  if (
    any(bmi_transitions$age_t < 20L) ||
    any(bmi_transitions$age_t > 84L)
  ) {
    stop(
      "`bmi_transitions$age_t` must contain values from 20 to 84.",
      call. = FALSE
    )
  }

  sex <- as.character(
    bmi_transitions$sex
  )

  if (
    any(is.na(sex)) ||
    any(!sex %in% c("men", "women"))
  ) {
    stop(
      "`bmi_transitions$sex` must contain only `men` and `women`.",
      call. = FALSE
    )
  }

  education_level2 <- as.character(
    bmi_transitions$education_level2
  )

  if (
    any(is.na(education_level2)) ||
    any(!nzchar(education_level2))
  ) {
    stop(
      "`bmi_transitions$education_level2` must contain no missing or empty values.",
      call. = FALSE
    )
  }

  period_values <- sort(
    unique(
      as.integer(
        bmi_transitions$year_t
      )
    )
  )

  if (length(period_values) < 2L) {
    stop(
      "`bmi_transitions` must contain at least two baseline survey years to estimate a period effect.",
      call. = FALSE
    )
  }

  if (is.null(projection_period)) {
    projection_period <- max(
      period_values
    )
  }

  if (
    !is.numeric(projection_period) ||
    length(projection_period) != 1L ||
    is.na(projection_period) ||
    !is.finite(projection_period) ||
    projection_period != as.integer(projection_period)
  ) {
    stop(
      "`projection_period` must be one finite integer value.",
      call. = FALSE
    )
  }

  projection_period <- as.integer(
    projection_period
  )

  if (
    !projection_period %in%
    period_values
  ) {
    stop(
      "`projection_period` must be represented in `bmi_transitions$year_t`.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(transition_limits) ||
    length(transition_limits) != 2L ||
    any(is.na(transition_limits)) ||
    any(!is.finite(transition_limits)) ||
    transition_limits[1L] >= transition_limits[2L]
  ) {
    stop(
      "`transition_limits` must contain two finite values in increasing order.",
      call. = FALSE
    )
  }

  transition_weight <- bmi_transitions$transition_weight

  if (
    !is.numeric(transition_weight) ||
    any(is.na(transition_weight)) ||
    any(!is.finite(transition_weight)) ||
    any(transition_weight <= 0)
  ) {
    stop(
      "`bmi_transitions$transition_weight` must contain positive, finite numeric values.",
      call. = FALSE
    )
  }

  model_data <- data.frame(
    annual_bmi_change = as.numeric(
      bmi_transitions$annual_bmi_change
    ),
    sex = factor(
      sex,
      levels = c(
        "men",
        "women"
      )
    ),
    age_t = as.numeric(
      bmi_transitions$age_t
    ),
    bmi_t = as.numeric(
      bmi_transitions$bmi_t
    ),
    education_level2 = factor(
      education_level2
    ),
    period = factor(
      as.integer(
        bmi_transitions$year_t
      )
    ),
    transition_weight = as.numeric(transition_weight)
  )

  # bmi_t is intentionally excluded. In synthetic-cohort data, the negative
  # bmi_t coefficient arises from differential mortality (obese individuals
  # dying between survey waves lower the surviving group mean). Mortality is
  # already handled by the simulation's mortality module, so including bmi_t
  # here would double-count that effect and incorrectly cause obese simulated
  # individuals to lose BMI over time.
  fitted_model <- stats::lm(
    annual_bmi_change ~
      sex +
      age_t +
      I(age_t ^ 2) +
      education_level2 +
      period,
    data = model_data,
    weights = transition_weight
  )

  if (fitted_model$rank < length(
    stats::coef(
      fitted_model
    )
  )) {
    stop(
      "The BMI-transition model is rank deficient. ",
      "Provide transitions with sufficient variation in the model predictors.",
      call. = FALSE
    )
  }

  if (stats::df.residual(
    fitted_model
  ) <= 0L) {
    stop(
      "The BMI-transition model requires residual degrees of freedom greater than zero.",
      call. = FALSE
    )
  }

  residual_rmse <- sqrt(
    mean(
      stats::residuals(
        fitted_model
      ) ^ 2
    )
  )

  structure(
    list(
      model = fitted_model,
      residual_rmse = as.numeric(
        residual_rmse
      ),
      projection_period = projection_period,
      transition_limits = as.numeric(
        transition_limits
      )
    ),
    class = "simeq_bmi_transition_model"
  )
}
