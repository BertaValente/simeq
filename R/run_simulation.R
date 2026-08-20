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
#' @param bmi_transition_model Optional legacy `ponte_bmi_transition_model`
#'   object. Required only when `bmi_update_method = "legacy"`.
#' @param hayes_bmi_model Optional output from
#'   [ponte_fit_bmi_transition_hayes()]. Required when
#'   `bmi_update_method = "historical"`.
#' @param bmi_update_method BMI update strategy: `"legacy"` or `"historical"`.
#' @param min_cell_n Minimum NHS sample size required at both endpoints of an
#'   empirical BMI-transition cell.
#' @param stochastic_bmi Logical; whether to add individual random BMI
#'   variation during historical updates.
#' @param donor_pool A validated historical entrant donor pool used for all
#'   entry years unless `donor_pools` is supplied.
#' @param donor_pools Optional named list of period-specific donor pools.
#'   Each element must be a list with integer fields `from` and `to`
#'   (inclusive entry-year range) and a data frame field `pool`. When
#'   supplied, the pool whose range brackets the current entry year is used;
#'   `donor_pool` acts as a fallback if no range matches.
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
    donor_pool,
    donor_pools = NULL,
    entrant_targets,
    bmi_transition_model = NULL,
    hayes_bmi_model = NULL,
    bmi_update_method = c("legacy", "historical"),
    min_cell_n = 30L,
    stochastic_bmi = TRUE,
    education_source = c("existing", "map_3_to_2"),
    bmi_limits = c(10, 80)
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

  bmi_update_method <- match.arg(bmi_update_method)

  if (
    identical(bmi_update_method, "historical") &&
    is.null(hayes_bmi_model)
  ) {
    stop(
      "`hayes_bmi_model` must be supplied when ",
      "`bmi_update_method = 'historical'`.",
      call. = FALSE
    )
  }

  if (
    identical(bmi_update_method, "legacy") &&
    is.null(bmi_transition_model)
  ) {
    stop(
      "`bmi_transition_model` must be supplied when ",
      "`bmi_update_method = 'legacy'`.",
      call. = FALSE
    )
  }

  ponte_validate_entrant_donor_pool(
    donor_pool = donor_pool,
    entry_age = settings$entry_age
  )

  # Helper: select the period-appropriate donor pool for a given entry year.
  .select_pool <- function(entry_year) {
    if (!is.null(donor_pools)) {
      for (dp in donor_pools) {
        if (entry_year >= dp$from && entry_year <= dp$to) {
          return(dp$pool)
        }
      }
    }
    donor_pool
  }

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
          donor_pool = .select_pool(entry_year),
          entrant_targets = annual_targets,
          n_entrants = settings$entrant_n_per_year,
          hayes_bmi_model = hayes_bmi_model,
          bmi_update_method = bmi_update_method,
          min_cell_n = min_cell_n,
          stochastic_bmi = stochastic_bmi,
          education_source = education_source,
          entry_age = settings$entry_age,
          max_age = settings$max_age,
          bmi_limits = bmi_limits
        )

        state <- annual_result$living_state

        annual_records[[
          length(annual_records) + 1L
        ]] <- dplyr::bind_rows(
          annual_result$living_state,
          annual_result$deaths
        )
      }
    }

    simulation_results[[
      simulation_number
    ]] <- dplyr::bind_rows(
      annual_records
    )
  }

  simulation_output <- dplyr::bind_rows(
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
