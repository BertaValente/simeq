#' Update BMI using historical period-specific NHS transitions
#'
#' Applies POND2-weighted, annual BMI changes estimated from the NHS for the
#' relevant historical period. Cells with insufficient observed respondents use
#' the fitted Hayes regression prediction instead.
#'
#' @param population Synthetic population.
#' @param year Current simulation year.
#' @param hayes_bmi_model Output from ponte_fit_bmi_transition_hayes().
#' @param min_cell_n Minimum NHS sample size required in both survey waves.
#' @param stochastic Logical; add individual-level residual variation.
#' @param min_bmi Minimum allowed BMI.
#' @param max_bmi Maximum allowed BMI.
#'
#' @return Synthetic population with updated BMI and transition diagnostics.
#' @export
ponte_update_bmi_historical <- function(
    population,
    year,
    hayes_bmi_model,
    min_cell_n = 30L,
    stochastic = TRUE,
    min_bmi = 10,
    max_bmi = 80
) {
  required_population_columns <- c(
    "sex",
    "age",
    "education_level2",
    "bmi"
  )

  missing_population_columns <- setdiff(
    required_population_columns,
    names(population)
  )

  if (length(missing_population_columns) > 0L) {
    stop(
      "Population is missing required columns: ",
      paste(missing_population_columns, collapse = ", "),
      call. = FALSE
    )
  }

  required_model_components <- c(
    "model",
    "annual_changes",
    "residual_rmse"
  )

  missing_model_components <- setdiff(
    required_model_components,
    names(hayes_bmi_model)
  )

  if (length(missing_model_components) > 0L) {
    stop(
      "Hayes BMI model is missing: ",
      paste(missing_model_components, collapse = ", "),
      call. = FALSE
    )
  }

  transition_periods <- hayes_bmi_model$annual_changes |>
    dplyr::distinct(
      year_t,
      year_t1,
      interval_years
    ) |>
    dplyr::arrange(year_t)

  period <- transition_periods |>
    dplyr::filter(
      year >= year_t,
      year < year_t1
    )

  # For years beyond the last observed NHS wave, extrapolate using the most
  # recent transition period with Hayes regression predictions only.
  use_regression_only <- FALSE

  if (nrow(period) != 1L) {
    latest_year_t1 <- max(transition_periods$year_t1, na.rm = TRUE)
    if (year >= latest_year_t1) {
      period <- transition_periods |>
        dplyr::filter(year_t == max(year_t)) |>
        dplyr::slice(1L)
      use_regression_only <- TRUE
    } else {
      stop(
        "No unique historical BMI transition period is available for year ",
        year,
        ".",
        call. = FALSE
      )
    }
  }

  transition_data <- hayes_bmi_model$annual_changes |>
    dplyr::filter(
      year_t == period$year_t,
      year_t1 == period$year_t1
    ) |>
    dplyr::mutate(
      use_empirical_change =
        sample_n_t >= min_cell_n &
        sample_n_t1 >= min_cell_n
    ) |>
    dplyr::select(
      sex,
      age_group,
      bmi_category,
      empirical_annual_bmi_change = annual_bmi_change,
      sample_n_t,
      sample_n_t1,
      use_empirical_change
    )

  model_data <- population |>
    dplyr::transmute(
      sex = as.character(sex),
      age = as.integer(age),
      bmi = as.numeric(bmi),
      education_level2 = education_level2
    )

  regression_change <- as.numeric(
    stats::predict(
      object = hayes_bmi_model$model,
      newdata = model_data
    )
  )

  bmi_category <- dplyr::case_when(
    population$bmi < 18.5 ~ "underweight",
    population$bmi < 25 ~ "normal_weight",
    population$bmi < 30 ~ "overweight",
    population$bmi < 35 ~ "obesity_I",
    population$bmi < 40 ~ "obesity_II",
    TRUE ~ "obesity_III"
  )

  age_group <- cut(
    population$age,
    breaks = seq(20, 85, by = 5),
    right = FALSE,
    include.lowest = TRUE,
    labels = paste(
      seq(20, 80, by = 5),
      seq(24, 84, by = 5),
      sep = "_"
    )
  )

  person_transition_data <- tibble::tibble(
    sex = as.character(population$sex),
    age_group = as.character(age_group),
    bmi_category = bmi_category
  ) |>
    dplyr::left_join(
      transition_data,
      by = c("sex", "age_group", "bmi_category")
    )

  expected_change <- dplyr::if_else(
    person_transition_data$use_empirical_change %in% TRUE,
    person_transition_data$empirical_annual_bmi_change,
    regression_change
  )

  transition_source <- dplyr::if_else(
    person_transition_data$use_empirical_change %in% TRUE,
    "empirical",
    "regression_fallback"
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
      bmi_transition_year = year,
      bmi_transition_period = paste0(
        period$year_t,
        "-",
        period$year_t1
      ),
      bmi_category_before_update = bmi_category,
      expected_annual_bmi_change = expected_change,
      random_annual_bmi_change = random_change,
      annual_bmi_change =
        expected_annual_bmi_change +
        random_annual_bmi_change,
      bmi_transition_source = transition_source,
      bmi = pmin(
        max_bmi,
        pmax(
          min_bmi,
          bmi + annual_bmi_change
        )
      ),
      bmi_category = ponte_bmi_category(bmi),
      obesity = bmi >= 30
    )
}
