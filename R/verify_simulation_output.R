#' Verify a SIMEQ simulation output
#'
#' Performs structural checks on person-year simulation output, including
#' person-year uniqueness, death-record consistency, age progression, BMI
#' consistency, entrant ages, and optional entrant-target calibration.
#'
#' @param simulation_output A data frame returned by
#'   [simeq_run_simulation()].
#' @param entrant_targets Optional annual entrant-target table.
#' @param entry_age Integer age at which new entrants join the simulation.
#' @param max_age Integer maximum represented age.
#' @param tolerance Numeric tolerance used for entrant-weight comparisons.
#'
#' @return A data frame containing one row per verification check.
#' @export
simeq_verify_simulation_output <- function(
    simulation_output,
    entrant_targets = NULL,
    entry_age = 20L,
    max_age = 84L,
    tolerance = 1e-8
) {
  if (!is.data.frame(simulation_output)) {
    stop(
      "`simulation_output` must be a data frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "person_id",
    "simulation",
    "year",
    "baseline_year",
    "age",
    "sex",
    "education_level",
    "education_level2",
    "survey_weight",
    "bmi",
    "bmi_category",
    "obesity",
    "alive",
    "death_year",
    "age_at_death"
  )

  missing_columns <- setdiff(
    required_columns,
    names(simulation_output)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  if (nrow(simulation_output) == 0L) {
    stop(
      "`simulation_output` must contain at least one record.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(entry_age) ||
    length(entry_age) != 1L ||
    is.na(entry_age) ||
    !is.finite(entry_age) ||
    entry_age != as.integer(entry_age)
  ) {
    stop(
      "`entry_age` must be one finite integer value.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(max_age) ||
    length(max_age) != 1L ||
    is.na(max_age) ||
    !is.finite(max_age) ||
    max_age != as.integer(max_age) ||
    max_age <= entry_age
  ) {
    stop(
      "`max_age` must be one finite integer value greater than `entry_age`.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(tolerance) ||
    length(tolerance) != 1L ||
    is.na(tolerance) ||
    !is.finite(tolerance) ||
    tolerance < 0
  ) {
    stop(
      "`tolerance` must be one non-negative finite value.",
      call. = FALSE
    )
  }

  person_year_identifier <- paste(
    simulation_output$simulation,
    simulation_output$person_id,
    simulation_output$year,
    sep = "::"
  )

  person_year_unique <- anyDuplicated(
    person_year_identifier
  ) == 0L

  living_records_valid <- all(
    (
      simulation_output$alive &
        is.na(simulation_output$death_year) &
        is.na(simulation_output$age_at_death)
    ) |
      (
        !simulation_output$alive &
          !is.na(simulation_output$death_year) &
          !is.na(simulation_output$age_at_death)
      )
  )

  death_records <- simulation_output[
    !simulation_output$alive,
    ,
    drop = FALSE
  ]

  death_records_consistent <- all(
    death_records$death_year ==
      death_records$year
  ) &&
    all(
      death_records$age_at_death ==
        death_records$age
    )

  death_identifier <- paste(
    simulation_output$simulation,
    simulation_output$person_id,
    sep = "::"
  )

  death_count <- tapply(
    !simulation_output$alive,
    death_identifier,
    sum
  )

  no_multiple_deaths <- all(
    death_count <= 1L
  )

  output_order <- order(
    simulation_output$simulation,
    simulation_output$person_id,
    simulation_output$year
  )

  ordered_output <- simulation_output[
    output_order,
    ,
    drop = FALSE
  ]

  ordered_identifier <- paste(
    ordered_output$simulation,
    ordered_output$person_id,
    sep = "::"
  )

  split_indices <- split(
    seq_len(
      nrow(ordered_output)
    ),
    ordered_identifier
  )

  history_checks <- lapply(
    split_indices,
    function(indices) {
      person_history <- ordered_output[
        indices,
        ,
        drop = FALSE
      ]

      if (nrow(person_history) <= 1L) {
        return(
          c(
            years_consecutive = TRUE,
            age_progression = TRUE,
            no_record_after_death = TRUE
          )
        )
      }

      year_difference <- diff(
        person_history$year
      )

      years_consecutive <- all(
        year_difference == 1L
      )

      previous_age <- utils::head(
        person_history$age,
        -1L
      )

      current_age <- utils::tail(
        person_history$age,
        -1L
      )

      current_alive <- utils::tail(
        person_history$alive,
        -1L
      )

      expected_age <- ifelse(
        current_alive,
        pmin(
          previous_age + 1L,
          as.integer(max_age)
        ),
        previous_age
      )

      age_progression <- all(
        current_age == expected_age
      )

      death_positions <- which(
        !person_history$alive
      )

      no_record_after_death <- length(
        death_positions
      ) == 0L ||
        max(death_positions) == nrow(
          person_history
        )

      c(
        years_consecutive = years_consecutive,
        age_progression = age_progression,
        no_record_after_death = no_record_after_death
      )
    }
  )

  history_check_matrix <- do.call(
    rbind,
    history_checks
  )

  years_consecutive <- all(
    history_check_matrix[
      ,
      "years_consecutive"
    ]
  )

  age_progression <- all(
    history_check_matrix[
      ,
      "age_progression"
    ]
  )

  no_record_after_death <- all(
    history_check_matrix[
      ,
      "no_record_after_death"
    ]
  )

  bmi_in_range <- all(
    is.finite(simulation_output$bmi) &
      simulation_output$bmi >= 10 &
      simulation_output$bmi <= 80
  )

  bmi_category_consistent <- identical(
    as.character(
      simulation_output$bmi_category
    ),
    simeq_bmi_category(
      simulation_output$bmi
    )
  )

  obesity_consistent <- identical(
    as.logical(
      simulation_output$obesity
    ),
    simulation_output$bmi >= 30
  )

  entrant_records <- simulation_output[
    grepl(
      "^entrant_",
      simulation_output$person_id
    ) &
      simulation_output$year ==
      simulation_output$baseline_year,
    ,
    drop = FALSE
  ]

  entrant_age_consistent <- nrow(
    entrant_records
  ) == 0L ||
    all(
      entrant_records$age == entry_age
    )

  entrant_weights_match_targets <- NA

  if (!is.null(entrant_targets)) {
    simeq_validate_entrant_targets(
      entrant_targets = entrant_targets,
      start_year = min(
        entrant_targets$entry_year
      ),
      end_year = max(
        entrant_targets$entry_year
      )
    )

    simulated_entrant_weights <- stats::aggregate(
      survey_weight ~ simulation + year + sex,
      data = entrant_records,
      FUN = sum
    )

    expected_entrant_weights <- merge(
      expand.grid(
        simulation = sort(
          unique(
            simulation_output$simulation
          )
        ),
        entry_year = sort(
          unique(
            entrant_targets$entry_year
          )
        ),
        sex = c(
          "men",
          "women"
        ),
        stringsAsFactors = FALSE
      ),
      entrant_targets,
      by = c(
        "entry_year",
        "sex"
      ),
      all.x = TRUE
    )

    comparison <- merge(
      expected_entrant_weights,
      simulated_entrant_weights,
      by.x = c(
        "simulation",
        "entry_year",
        "sex"
      ),
      by.y = c(
        "simulation",
        "year",
        "sex"
      ),
      all.x = TRUE
    )

    comparison$survey_weight[
      is.na(
        comparison$survey_weight
      )
    ] <- 0

    entrant_weights_match_targets <- all(
      abs(
        comparison$survey_weight -
          comparison$entrant_population
      ) <= tolerance
    )
  }

  data.frame(
    check = c(
      "Person-year uniqueness",
      "Living and death record fields",
      "Death-year and death-age consistency",
      "No multiple death records",
      "Consecutive calendar years",
      "Age progression",
      "No records after death",
      "BMI within range",
      "BMI-category consistency",
      "Obesity consistency",
      "Entrants begin at entry age",
      "Entrant weights match targets"
    ),
    passed = c(
      person_year_unique,
      living_records_valid,
      death_records_consistent,
      no_multiple_deaths,
      years_consecutive,
      age_progression,
      no_record_after_death,
      bmi_in_range,
      bmi_category_consistent,
      obesity_consistent,
      entrant_age_consistent,
      entrant_weights_match_targets
    ),
    stringsAsFactors = FALSE
  )
}
