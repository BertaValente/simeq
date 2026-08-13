#' Run a PONTE-PT-O microsimulation
#'
#' Runs one or more annual open-cohort microsimulations from an initial
#' population through the requested projection end year.
#'
#' @param population A baseline population data frame.
#' @param settings A `ponte_settings` object.
#' @param life_table A validated life table with `age`, `sex`, and `qx`.
#' @param rr_bmi A validated BMI mortality relative-risk table.
#' @param rr_education A validated education mortality relative-risk table.
#' @param bmi_transition_model A `ponte_bmi_transition_model` object.
#' @param donor_pool A validated historical entrant donor pool.
#' @param entrant_targets A validated annual entrant-target table.
#' @param education_source Either `"existing"` or `"map_3_to_2"`.
#' @param bmi_limits Numeric vector of lower and upper permitted BMI values.
#'
#' @return A data frame containing living and death records for every
#'   simulation year.
#' @export
ponte_run_simulation <- function(
    population,
    settings,
    life_table,
    rr_bmi,
    rr_education,
    bmi_transition_model,
    donor_pool,
    entrant_targets,
    education_source = c(
      "existing",
      "map_3_to_2"
    ),
    bmi_limits = c(
      10,
      80
    )
) {
  if (!inherits(settings, "ponte_settings")) {
    stop(
      "`settings` must be a `ponte_settings` object.",
      call. = FALSE
    )
  }

  education_source <- match.arg(
    education_source
  )

  ponte_validate_entrant_donor_pool(
    donor_pool = donor_pool,
    entry_age = settings$entry_age
  )

  initial_state <- ponte_initialise_population(
    population = population,
    settings = settings
  )

  if (
    settings$projection_end_year >
    settings$initial_year
  ) {
    ponte_validate_entrant_targets(
      entrant_targets = entrant_targets,
      start_year = as.integer(
        settings$initial_year + 1L
      ),
      end_year = settings$projection_end_year
    )
  }

  set.seed(
    settings$seed
  )

  simulation_results <- vector(
    mode = "list",
    length = settings$n_simulations
  )

  for (simulation_number in seq_len(
    settings$n_simulations
  )) {
    state <- initial_state

    state$simulation <- rep(
      as.integer(simulation_number),
      nrow(state)
    )

    annual_records <- list(
      state
    )

    if (
      settings$projection_end_year >
      settings$initial_year
    ) {
      for (entry_year in seq.int(
        settings$initial_year + 1L,
        settings$projection_end_year
      )) {
        annual_targets <- entrant_targets[
          entrant_targets$entry_year == entry_year,
          ,
          drop = FALSE
        ]

        annual_result <- ponte_advance_one_year(
          state = state,
          life_table = life_table,
          rr_bmi = rr_bmi,
          rr_education = rr_education,
          bmi_transition_model = bmi_transition_model,
          donor_pool = donor_pool,
          entrant_targets = annual_targets,
          n_entrants = settings$entrant_n_per_year,
          education_source = education_source,
          entry_age = settings$entry_age,
          max_age = settings$max_age,
          bmi_limits = bmi_limits
        )

        state <- annual_result$living_state

        annual_records[[
          length(annual_records) + 1L
        ]] <- rbind(
          annual_result$living_state,
          annual_result$deaths
        )
      }
    }

    simulation_results[[
      simulation_number
    ]] <- do.call(
      rbind,
      annual_records
    )
  }

  simulation_output <- do.call(
    rbind,
    simulation_results
  )

  simulation_output <- simulation_output[
    order(
      simulation_output$simulation,
      simulation_output$person_id,
      simulation_output$year,
      simulation_output$alive,
      decreasing = FALSE
    ),
    ,
    drop = FALSE
  ]

  rownames(
    simulation_output
  ) <- NULL

  simulation_output
}
