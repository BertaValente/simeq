#' Update BMI using the SIMEQ-PT Hayes-style transition model
#'
#' @param population Synthetic population.
#' @param hayes_bmi_model Fitted object from simeq_fit_bmi_transition_hayes().
#' @param stochastic Logical; add annual individual-level random variation.
#' @param min_bmi Minimum allowable BMI.
#' @param max_bmi Maximum allowable BMI.
#'
#' @return Synthetic population with updated BMI.
#' @export
simeq_update_bmi_hayes <- function(
    population,
    hayes_bmi_model,
    stochastic = TRUE,
    min_bmi = 10,
    max_bmi = 80
) {
  required_columns <- c(
    "sex",
    "age",
    "education_level2",
    "bmi"
  )

  missing_columns <- setdiff(required_columns, names(population))

  if (length(missing_columns) > 0L) {
    stop(
      "Population is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(hayes_bmi_model$model)) {
    stop(
      "`hayes_bmi_model` must contain a fitted model.",
      call. = FALSE
    )
  }

  if (is.null(hayes_bmi_model$residual_rmse)) {
    stop(
      "`hayes_bmi_model` must contain `residual_rmse`.",
      call. = FALSE
    )
  }

  model_data <- population |>
    dplyr::transmute(
      sex = sex,
      age = age,
      bmi = bmi,
      education_level2 = education_level2
    )

  expected_change <- as.numeric(
    stats::predict(
      object = hayes_bmi_model$model,
      newdata = model_data
    )
  )

  random_change <- rep(0, nrow(population))

  if (isTRUE(stochastic)) {
    random_change <- stats::rnorm(
      n = nrow(population),
      mean = 0,
      sd = hayes_bmi_model$residual_rmse
    )
  }

  population |>
    dplyr::mutate(
      expected_annual_bmi_change = expected_change,
      random_annual_bmi_change = random_change,
      annual_bmi_change =
        expected_annual_bmi_change +
        random_annual_bmi_change,
      bmi = pmin(
        max_bmi,
        pmax(min_bmi, bmi + annual_bmi_change)
      ),
      bmi_category = simeq_bmi_category(bmi),
      obesity = bmi >= 30
    ) |>
    .refresh_weight_from_bmi()
}
