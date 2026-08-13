#' Calculate QALYs from EQ-5D norms and BMI category
#'
#' Calculates annual QALYs for living people aged 20 to 84 years using
#' age-sex EQ-5D norms, education adjustments, and BMI utility decrements.
#'
#' @param simulation_output A data frame returned by
#'   [ponte_run_simulation()].
#' @param min_age Minimum age included in QALY calculations.
#' @param max_age Maximum age included in QALY calculations.
#'
#' @return A list containing individual QALY records, annual results by
#'   simulation, and annual summaries across simulations.
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

  missing_columns <- setdiff(
    required_columns,
    names(simulation_output)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

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
      delta_utility = numeric(),
      utility_with_bmi = numeric(),
      qaly_weighted = numeric(),
      stringsAsFactors = FALSE
    )

    empty_by_year <- data.frame(
      simulation = integer(),
      year = integer(),
      sex = character(),
      education_level = character(),
      total_qaly_weighted = numeric(),
      stringsAsFactors = FALSE
    )

    return(
      list(
        individual = empty_individual,
        by_year_sim = empty_by_year,
        by_year = empty_by_year
      )
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

  education_delta <- rep(
    NA_real_,
    nrow(living_population)
  )

  education_delta[
    living_population$education_level == "low"
  ] <- 0.848 - 0.887

  education_delta[
    living_population$education_level == "medium"
  ] <- 0.912 - 0.887

  education_delta[
    living_population$education_level == "high"
  ] <- 0.930 - 0.887

  bmi_category <- ponte_harmonise_bmi_category(
    living_population$bmi_category
  )

  delta_utility <- rep(
    0,
    nrow(living_population)
  )

  delta_utility[
    bmi_category == "overweight"
  ] <- -0.020

  delta_utility[
    bmi_category == "obesity_I"
  ] <- -0.047

  delta_utility[
    bmi_category == "obesity_II"
  ] <- -0.072

  delta_utility[
    bmi_category == "obesity_III"
  ] <- -0.084

  eq5d_norm <- age_norm + education_delta

  utility_with_bmi <- pmin(
    pmax(
      eq5d_norm + delta_utility,
      0
    ),
    1
  )

  individual <- living_population

  individual$bmi_category <- bmi_category
  individual$eq5d_norm <- eq5d_norm
  individual$delta_utility <- delta_utility
  individual$utility_with_bmi <- utility_with_bmi
  individual$qaly_weighted <- utility_with_bmi *
    individual$survey_weight
  individual$obesity_status <- ifelse(
    individual$bmi_category %in% c(
      "obesity_I",
      "obesity_II",
      "obesity_III"
    ),
    "obesity",
    "no_obesity"
  )

  by_year_sim <- stats::aggregate(
    qaly_weighted ~ simulation + year + sex + education_level,
    data = individual,
    FUN = sum,
    na.rm = TRUE
  )

  names(by_year_sim)[
    names(by_year_sim) == "qaly_weighted"
  ] <- "total_qaly_weighted"

  by_year_obesity_sim <- stats::aggregate(
    cbind(
      N_weighted = survey_weight,
      total_qaly_weighted = qaly_weighted
    ) ~ simulation + year + sex + education_level + obesity_status,
    data = individual,
    FUN = sum,
    na.rm = TRUE
  )

  by_year_obesity_sim$mean_qaly_per_person <- ifelse(
    by_year_obesity_sim$N_weighted > 0,
    by_year_obesity_sim$total_qaly_weighted /
      by_year_obesity_sim$N_weighted,
    NA_real_
  )

  obesity_summary_keys <- paste(
    by_year_obesity_sim$year,
    by_year_obesity_sim$sex,
    by_year_obesity_sim$education_level,
    by_year_obesity_sim$obesity_status,
    sep = "\r"
  )

  obesity_summary_rows <- lapply(
    unique(obesity_summary_keys),
    function(key) {
      index <- which(obesity_summary_keys == key)

      group_data <- by_year_obesity_sim[
        index,
        ,
        drop = FALSE
      ]

      data.frame(
        year = group_data$year[1],
        sex = group_data$sex[1],
        education_level =
          group_data$education_level[1],
        obesity_status =
          group_data$obesity_status[1],
        mean_qaly_per_person = mean(
          group_data$mean_qaly_per_person,
          na.rm = TRUE
        ),
        qaly_lo = as.numeric(
          stats::quantile(
            group_data$mean_qaly_per_person,
            0.025,
            na.rm = TRUE
          )
        ),
        qaly_hi = as.numeric(
          stats::quantile(
            group_data$mean_qaly_per_person,
            0.975,
            na.rm = TRUE
          )
        ),
        stringsAsFactors = FALSE
      )
    }
  )

  by_year_obesity <- do.call(
    rbind,
    obesity_summary_rows
  )
  summary_keys <- paste(
    by_year_sim$year,
    by_year_sim$sex,
    by_year_sim$education_level,
    sep = "\r"
  )

  summary_rows <- lapply(
    unique(summary_keys),
    function(key) {
      index <- which(summary_keys == key)

      group_data <- by_year_sim[
        index,
        ,
        drop = FALSE
      ]

      data.frame(
        year = group_data$year[1],
        sex = group_data$sex[1],
        education_level =
          group_data$education_level[1],
        mean_qaly = mean(
          group_data$total_qaly_weighted,
          na.rm = TRUE
        ),
        qaly_lo = as.numeric(
          stats::quantile(
            group_data$total_qaly_weighted,
            0.025,
            na.rm = TRUE
          )
        ),
        qaly_hi = as.numeric(
          stats::quantile(
            group_data$total_qaly_weighted,
            0.975,
            na.rm = TRUE
          )
        ),
        stringsAsFactors = FALSE
      )
    }
  )

  by_year <- do.call(
    rbind,
    summary_rows
  )

  list(
    individual = individual,
    by_year_sim = by_year_sim,
    by_year = by_year,
    by_year_obesity_sim = by_year_obesity_sim,
    by_year_obesity = by_year_obesity
  )
}
