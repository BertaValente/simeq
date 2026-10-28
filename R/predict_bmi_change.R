#' Predict annual individual BMI change
#'
#' Predicts annual BMI change for a living population state using a fitted
#' BMI-transition model. Individual residual variation is added, then each
#' simulated BMI change is limited to the model's transition bounds.
#'
#' @param state A valid living SIMEQ population state containing
#'   `education_level2`.
#' @param bmi_transition_model A `simeq_bmi_transition_model` object returned
#'   by [simeq_fit_bmi_transition()].
#' @param period Optional survey period (a baseline survey year used when the
#'   model was fitted) to predict with. Default `NULL` uses the model's
#'   `projection_period`, the latest period.
#'
#' @return A numeric vector of simulated annual BMI changes, in the same row
#'   order as `state`.
#' @export
simeq_predict_bmi_change <- function(
    state,
    bmi_transition_model,
    period = NULL
) {
  simeq_validate_living_state(
    state = state
  )

  if (
    !"education_level2" %in%
    names(state)
  ) {
    stop(
      "`state` must contain `education_level2`.",
      call. = FALSE
    )
  }

  state <- simeq_prepare_education_level2(
    state = state,
    source = "existing"
  )

  if (
    !inherits(
      bmi_transition_model,
      "simeq_bmi_transition_model"
    )
  ) {
    stop(
      "`bmi_transition_model` must be a `simeq_bmi_transition_model` object.",
      call. = FALSE
    )
  }

  if (
    is.null(bmi_transition_model$model) ||
    !inherits(
      bmi_transition_model$model,
      "lm"
    )
  ) {
    stop(
      "`bmi_transition_model$model` must be an `lm` object.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(
      bmi_transition_model$residual_rmse
    ) ||
    length(
      bmi_transition_model$residual_rmse
    ) != 1L ||
    is.na(
      bmi_transition_model$residual_rmse
    ) ||
    !is.finite(
      bmi_transition_model$residual_rmse
    ) ||
    bmi_transition_model$residual_rmse < 0
  ) {
    stop(
      "`bmi_transition_model$residual_rmse` must be one non-negative finite value.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(
      bmi_transition_model$projection_period
    ) ||
    length(
      bmi_transition_model$projection_period
    ) != 1L ||
    is.na(
      bmi_transition_model$projection_period
    ) ||
    !is.finite(
      bmi_transition_model$projection_period
    ) ||
    bmi_transition_model$projection_period !=
    as.integer(
      bmi_transition_model$projection_period
    )
  ) {
    stop(
      "`bmi_transition_model$projection_period` must be one finite integer value.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(
      bmi_transition_model$transition_limits
    ) ||
    length(
      bmi_transition_model$transition_limits
    ) != 2L ||
    any(
      is.na(
        bmi_transition_model$transition_limits
      )
    ) ||
    any(
      !is.finite(
        bmi_transition_model$transition_limits
      )
    ) ||
    bmi_transition_model$transition_limits[1L] >=
    bmi_transition_model$transition_limits[2L]
  ) {
    stop(
      "`bmi_transition_model$transition_limits` must contain two finite values in increasing order.",
      call. = FALSE
    )
  }

  model_levels <- bmi_transition_model$model$xlevels

  required_model_levels <- c(
    "sex",
    "education_level2",
    "period"
  )

  if (
    !all(
      required_model_levels %in%
      names(model_levels)
    )
  ) {
    stop(
      "`bmi_transition_model$model` must contain factor levels for sex, education_level2, and period.",
      call. = FALSE
    )
  }

  projection_period <- as.integer(
    bmi_transition_model$projection_period
  )

  if (!is.null(period)) {
    if (length(period) != 1L || is.na(period)) {
      stop("`period` must be one survey year.", call. = FALSE)
    }
    projection_period <- as.integer(period)
  }

  prediction_data <- data.frame(
    sex = factor(
      as.character(state$sex),
      levels = model_levels$sex
    ),
    age_t = as.numeric(
      state$age
    ),
    bmi_t = as.numeric(
      state$bmi
    ),
    education_level2 = factor(
      as.character(state$education_level2),
      levels = model_levels$education_level2
    ),
    period = factor(
      rep(
        projection_period,
        nrow(state)
      ),
      levels = model_levels$period
    )
  )

  if (
    any(
      !stats::complete.cases(
        prediction_data
      )
    )
  ) {
    stop(
      "BMI-transition prediction data contain unsupported factor levels or missing values.",
      call. = FALSE
    )
  }

  predicted_change <- as.numeric(
    stats::predict(
      bmi_transition_model$model,
      newdata = prediction_data
    )
  )

  simulated_change <- stats::rnorm(
    n = nrow(state),
    mean = predicted_change,
    sd = bmi_transition_model$residual_rmse
  )

  pmin(
    pmax(
      simulated_change,
      bmi_transition_model$transition_limits[1L]
    ),
    bmi_transition_model$transition_limits[2L]
  )
}
