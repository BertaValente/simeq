#' Calculate annual QALYs from EQ-5D norms, education, and BMI category
#'
#' Calculates annual quality-adjusted life years (QALYs) for alive simulated
#' people aged 20 to 84 years. Individual utility is derived from age-specific
#' EQ-5D norms, education-level utility adjustments, and BMI-category utility
#' decrements. Population-level QALYs are calculated by applying each person's
#' survey weight.
#'
#' The current implementation uses age-specific, rather than sex-specific,
#' EQ-5D norms. No utility decrement is applied to underweight BMI in the base
#' case because the selected decrement inputs apply to overweight and obesity
#' categories only.
#'
#' @param simulation_output A data frame returned by [ponte_run_simulation()].
#' @param min_age Minimum age included in QALY calculations.
#' @param max_age Maximum age included in QALY calculations.
#'
#' @return A list containing:
#' \describe{
#'   \item{individual}{Individual annual utilities and weighted QALYs.}
#'   \item{by_year_sim}{Weighted QALY totals by simulation, year, sex, and education.}
#'   \item{by_year}{Mean weighted QALY totals and simulation quantiles by year, sex, and education.}
#'   \item{by_year_obesity_sim}{Weighted QALY totals and mean annual QALYs per person by obesity status.}
#'   \item{by_year_obesity}{Mean annual QALYs per person and simulation quantiles by obesity status.}
#' }
#'
#' @export
ponte_calculate_qalys <- function(
    simulation_output,
    min_age = 20L,
    max_age = 84L
) {
  required_columns <- c(
    "person_id",
    "simulation",
    "year",
    "age",
    "sex",
    "education_level",
    "survey_weight",
    "bmi_category",
    "alive"
  )

  missing_columns <- setdiff(required_columns, names(simulation_output))

  if (length(missing_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.numeric(min_age) || length(min_age) != 1L || is.na(min_age)) {
    stop("`min_age` must be one non-missing numeric value.", call. = FALSE)
  }

  if (!is.numeric(max_age) || length(max_age) != 1L || is.na(max_age)) {
    stop("`max_age` must be one non-missing numeric value.", call. = FALSE)
  }

  if (min_age > max_age) {
    stop("`min_age` cannot exceed `max_age`.", call. = FALSE)
  }

  empty_individual <- data.frame(
    person_id = character(),
    simulation = integer(),
    year = integer(),
    age = integer(),
    sex = character(),
    education_level = character(),
    survey_weight = numeric(),
    bmi_category = character(),
    alive = logical(),
    eq5d_norm = numeric(),
    education_utility_adjustment = numeric(),
    bmi_utility_decrement = numeric(),
    utility_with_bmi = numeric(),
    qaly_weighted = numeric(),
    obesity_status = character(),
    stringsAsFactors = FALSE
  )

  empty_by_year_sim <- data.frame(
    simulation = integer(),
    year = integer(),
    sex = character(),
    education_level = character(),
    weighted_population = numeric(),
    total_qaly_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    stringsAsFactors = FALSE
  )

  empty_by_year <- data.frame(
    year = integer(),
    sex = character(),
    education_level = character(),
    mean_weighted_population = numeric(),
    mean_total_qalys_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    qaly_total_lo = numeric(),
    qaly_total_hi = numeric(),
    qaly_per_person_lo = numeric(),
    qaly_per_person_hi = numeric(),
    stringsAsFactors = FALSE
  )

  empty_by_year_obesity_sim <- data.frame(
    simulation = integer(),
    year = integer(),
    sex = character(),
    education_level = character(),
    obesity_status = character(),
    weighted_population = numeric(),
    total_qaly_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    stringsAsFactors = FALSE
  )

  empty_by_year_obesity <- data.frame(
    year = integer(),
    sex = character(),
    education_level = character(),
    obesity_status = character(),
    mean_weighted_population = numeric(),
    mean_total_qalys_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    qaly_total_lo = numeric(),
    qaly_total_hi = numeric(),
    qaly_per_person_lo = numeric(),
    qaly_per_person_hi = numeric(),
    stringsAsFactors = FALSE
  )

  living_population <- simulation_output[
    !is.na(simulation_output$alive) &
      simulation_output$alive &
      !is.na(simulation_output$age) &
      simulation_output$age >= min_age &
      simulation_output$age <= max_age,
    ,
    drop = FALSE
  ]

  if (nrow(living_population) == 0L) {
    return(
      list(
        individual = empty_individual,
        by_year_sim = empty_by_year_sim,
        by_year = empty_by_year,
        by_year_obesity_sim = empty_by_year_obesity_sim,
        by_year_obesity = empty_by_year_obesity
      )
    )
  }

  if (anyNA(living_population$survey_weight) ||
      any(!is.finite(living_population$survey_weight)) ||
      any(living_population$survey_weight <= 0)) {
    stop(
      "Living simulated people must have finite, positive `survey_weight` values.",
      call. = FALSE
    )
  }

  valid_education <- c("low", "medium", "high")
  invalid_education <- setdiff(
    unique(as.character(living_population$education_level)),
    valid_education
  )

  if (length(invalid_education) > 0L) {
    stop(
      "Unrecognised education level(s): ",
      paste(invalid_education, collapse = ", "),
      ". Expected: ",
      paste(valid_education, collapse = ", "),
      call. = FALSE
    )
  }

  age_norm <- ifelse(
    living_population$age <= 29L,
    0.962,
    ifelse(
      living_population$age <= 49L,
      0.919,
      ifelse(
        living_population$age <= 69L,
        0.868,
        0.790
      )
    )
  )

  education_utility_adjustment <- unname(c(
    low = 0.848 - 0.887,
    medium = 0.912 - 0.887,
    high = 0.930 - 0.887
  )[as.character(living_population$education_level)])

  bmi_category <- ponte_harmonise_bmi_category(
    living_population$bmi_category
  )

  bmi_utility_decrement <- unname(c(
    underweight = 0,
    normal_weight = 0,
    overweight = -0.020,
    obesity_I = -0.047,
    obesity_II = -0.072,
    obesity_III = -0.084
  )[bmi_category])

  if (anyNA(bmi_utility_decrement)) {
    invalid_bmi <- unique(bmi_category[is.na(bmi_utility_decrement)])

    stop(
      "Unrecognised BMI category/categories after harmonisation: ",
      paste(invalid_bmi, collapse = ", "),
      call. = FALSE
    )
  }

  utility_with_bmi <- pmin(
    pmax(age_norm + education_utility_adjustment + bmi_utility_decrement, 0),
    1
  )

  individual <- living_population
  individual$bmi_category <- bmi_category
  individual$eq5d_norm <- age_norm
  individual$education_utility_adjustment <- education_utility_adjustment
  individual$bmi_utility_decrement <- bmi_utility_decrement
  individual$utility_with_bmi <- utility_with_bmi
  individual$qaly_weighted <- utility_with_bmi * individual$survey_weight
  individual$obesity_status <- ifelse(
    individual$bmi_category %in% c("obesity_I", "obesity_II", "obesity_III"),
    "obesity",
    "no_obesity"
  )

  by_year_sim <- stats::aggregate(
    cbind(
      weighted_population = survey_weight,
      total_qaly_weighted = qaly_weighted
    ) ~ simulation + year + sex + education_level,
    data = individual,
    FUN = sum,
    na.rm = TRUE
  )

  by_year_sim$mean_annual_qaly_per_person <- with(
    by_year_sim,
    ifelse(
      weighted_population > 0,
      total_qaly_weighted / weighted_population,
      NA_real_
    )
  )

  by_year_obesity_sim <- stats::aggregate(
    cbind(
      weighted_population = survey_weight,
      total_qaly_weighted = qaly_weighted
    ) ~ simulation + year + sex + education_level + obesity_status,
    data = individual,
    FUN = sum,
    na.rm = TRUE
  )

  by_year_obesity_sim$mean_annual_qaly_per_person <- with(
    by_year_obesity_sim,
    ifelse(
      weighted_population > 0,
      total_qaly_weighted / weighted_population,
      NA_real_
    )
  )

  summarise_across_simulations <- function(data, grouping_variables) {
    group_key <- do.call(
      paste,
      c(data[grouping_variables], sep = "\r")
    )

    summary_rows <- lapply(unique(group_key), function(key) {
      group_data <- data[group_key == key, , drop = FALSE]

      data.frame(
        group_data[grouping_variables][1, , drop = FALSE],
        mean_weighted_population = mean(
          group_data$weighted_population,
          na.rm = TRUE
        ),
        mean_total_qalys_weighted = mean(
          group_data$total_qaly_weighted,
          na.rm = TRUE
        ),
        mean_annual_qaly_per_person = mean(
          group_data$mean_annual_qaly_per_person,
          na.rm = TRUE
        ),
        qaly_total_lo = as.numeric(stats::quantile(
          group_data$total_qaly_weighted,
          probs = 0.025,
          na.rm = TRUE,
          names = FALSE
        )),
        qaly_total_hi = as.numeric(stats::quantile(
          group_data$total_qaly_weighted,
          probs = 0.975,
          na.rm = TRUE,
          names = FALSE
        )),
        qaly_per_person_lo = as.numeric(stats::quantile(
          group_data$mean_annual_qaly_per_person,
          probs = 0.025,
          na.rm = TRUE,
          names = FALSE
        )),
        qaly_per_person_hi = as.numeric(stats::quantile(
          group_data$mean_annual_qaly_per_person,
          probs = 0.975,
          na.rm = TRUE,
          names = FALSE
        )),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })

    do.call(rbind, summary_rows)
  }

  by_year <- summarise_across_simulations(
    data = by_year_sim,
    grouping_variables = c("year", "sex", "education_level")
  )

  by_year_obesity <- summarise_across_simulations(
    data = by_year_obesity_sim,
    grouping_variables = c(
      "year",
      "sex",
      "education_level",
      "obesity_status"
    )
  )

  rownames(by_year) <- NULL
  rownames(by_year_obesity) <- NULL

  list(
    individual = individual,
    by_year_sim = by_year_sim,
    by_year = by_year,
    by_year_obesity_sim = by_year_obesity_sim,
    by_year_obesity = by_year_obesity
  )
}
