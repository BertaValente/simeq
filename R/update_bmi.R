#' Update BMI in a living population state
#'
#' Applies simulated annual BMI changes to a living population state, limits
#' BMI to a plausible range, and recalculates BMI category and obesity status.
#'
#' @param state A valid living PONTE-PT-O population state containing
#'   `education_level2`.
#' @param bmi_transition_model A `ponte_bmi_transition_model` object returned
#'   by [ponte_fit_bmi_transition()].
#' @param bmi_limits Numeric vector of length two giving the lower and upper
#'   permitted BMI values.
#'
#' @return A living population state with updated BMI-derived variables.
#' @export
ponte_update_bmi <- function(
    state,
    bmi_transition_model,
    bmi_limits = c(
      10,
      80
    )
) {
  ponte_validate_living_state(
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

  bmi_change <- ponte_predict_bmi_change(
    state = state,
    bmi_transition_model = bmi_transition_model
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

  state$bmi_category <- ponte_bmi_category(
    state$bmi
  )

  state$obesity <- state$bmi >= 30

  state
}
