#' Update BMI in a living population state
#'
#' Applies simulated annual BMI changes to a living population state, limits
#' BMI to a plausible range, and recalculates BMI category and obesity status.
#'
#' @param state A valid living SIMEQ population state containing
#'   `education_level2`.
#' @param bmi_transition_model A `simeq_bmi_transition_model` object returned
#'   by [simeq_fit_bmi_transition()].
#' @param bmi_limits Numeric vector of length two giving the lower and upper
#'   permitted BMI values.
#' @param period Optional survey period passed to
#'   [simeq_predict_bmi_change()].
#'
#' @return A living population state with updated BMI-derived variables.
#' @export
simeq_update_bmi <- function(
    state,
    bmi_transition_model,
    bmi_limits = c(
      10,
      80
    ),
    period = NULL
) {
  simeq_validate_living_state(
    state = state
  )

  if (
    !is.numeric(bmi_limits) ||
    length(bmi_limits) != 2L ||
    any(is.na(bmi_limits)) ||
    any(!is.finite(bmi_limits)) ||
    bmi_limits[1L] >= bmi_limits[2L]
  ) {
    stop(
      "`bmi_limits` must contain two finite values in increasing order.",
      call. = FALSE
    )
  }

  bmi_change <- simeq_predict_bmi_change(
    state = state,
    bmi_transition_model = bmi_transition_model,
    period = period
  )

  updated_bmi <- pmin(
    pmax(
      state$bmi + bmi_change,
      bmi_limits[1L]
    ),
    bmi_limits[2L]
  )

  state$bmi <- as.numeric(
    updated_bmi
  )

  state$bmi_category <- simeq_bmi_category(
    state$bmi
  )

  state$obesity <- state$bmi >= 30

  .refresh_weight_from_bmi(state)
}

# Keep weight consistent with updated BMI. Height is held constant and may be
# stored in metres or centimetres (values above 3 are taken as cm); weight
# (kg) is back-calculated as BMI * (height_m)^2.
.refresh_weight_from_bmi <- function(state) {
  if ("height" %in% names(state) && "weight" %in% names(state)) {
    height_m <- ifelse(state$height > 3, state$height / 100, state$height)
    state$weight <- as.numeric(state$bmi * height_m^2)
  }

  state
}
