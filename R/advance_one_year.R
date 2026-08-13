#' Advance the population through one simulation year
#'
#' Applies annual mortality, updates BMI among survivors, advances survivor
#' ages and calendar year, and adds new age-20 entrants to form the next
#' living population state.
#'
#' @param state A valid living PONTE-PT-O population state for one simulation
#'   and one calendar year.
#' @param life_table A validated life table with `age`, `sex`, and `qx`.
#' @param rr_bmi A validated BMI mortality relative-risk table.
#' @param rr_education A validated education mortality relative-risk table.
#' @param bmi_transition_model A `ponte_bmi_transition_model` object.
#' @param donor_pool A validated historical entrant donor pool.
#' @param entrant_targets A validated entrant-target table.
#' @param n_entrants Integer number of microdata entrants to generate.
#' @param education_source Either `"existing"` or `"map_3_to_2"`.
#' @param entry_age Integer entrant age.
#' @param max_age Integer maximum represented age.
#' @param bmi_limits Numeric vector of lower and upper permitted BMI values.
#'
#' @return A named list containing `living_state`, `deaths`, and
#'   `mortality_probabilities`.
#' @export
ponte_advance_one_year <- function(
    state,
    life_table,
    rr_bmi,
    rr_education,
    bmi_transition_model,
    donor_pool,
    entrant_targets,
    n_entrants,
    education_source = c(
      "existing",
      "map_3_to_2"
    ),
    entry_age = 20L,
    max_age = 84L,
    bmi_limits = c(
      10,
      80
    )
) {
  ponte_validate_living_state(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  education_source <- match.arg(
    education_source
  )

  state_years <- unique(
    as.integer(state$year)
  )

  if (length(state_years) != 1L) {
    stop(
      "`state` must contain records from exactly one calendar year.",
      call. = FALSE
    )
  }

  simulation_values <- unique(
    as.integer(state$simulation)
  )

  if (length(simulation_values) != 1L) {
    stop(
      "`state` must contain records from exactly one simulation.",
      call. = FALSE
    )
  }

  current_year <- state_years[1L]

  next_year <- as.integer(
    current_year + 1L
  )

  mortality_result <- ponte_update_mortality(
    state = state,
    life_table = life_table,
    rr_bmi = rr_bmi,
    rr_education = rr_education,
    education_source = education_source,
    entry_age = entry_age,
    max_age = max_age
  )

  survivors <- mortality_result$living_state

  deaths <- mortality_result$deaths

  if (nrow(deaths) > 0L) {
    deaths$year <- rep(
      next_year,
      nrow(deaths)
    )
  }

  if (nrow(survivors) > 0L) {
    survivors <- ponte_update_bmi(
      state = survivors,
      bmi_transition_model = bmi_transition_model,
      bmi_limits = bmi_limits
    )

    survivors <- ponte_update_demography(
      state = survivors,
      entry_age = entry_age,
      max_age = max_age
    )
  }

  entrants <- ponte_generate_entrants(
    donor_pool = donor_pool,
    entrant_targets = entrant_targets,
    entry_year = next_year,
    simulation = simulation_values[1L],
    n_entrants = n_entrants,
    entry_age = entry_age
  )

  living_state <- rbind(
    survivors,
    entrants
  )

  ponte_validate_living_state(
    state = living_state,
    entry_age = entry_age,
    max_age = max_age
  )

  list(
    living_state = living_state,
    deaths = deaths,
    mortality_probabilities =
      mortality_result$mortality_probabilities
  )
}
