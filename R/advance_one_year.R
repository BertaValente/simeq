#' Advance the population through one simulation year
#'
#' Within each annual cycle, stochastic mortality is applied first, using each
#' person's BMI at the start of the year. BMI is then updated for survivors,
#' followed by demographic aging and the addition of new age-20 entrants to
#' form the next living population state. Deaths are recorded with their
#' start-of-year BMI.
#'
#' @param state A valid living SIMEQ population state for one simulation
#'   and one calendar year.
#' @param life_table A validated life table with `age`, `sex`, and `qx`.
#' @param rr_bmi A validated BMI mortality relative-risk table.
#' @param rr_education A validated education mortality relative-risk table.
#' @param bmi_transition_model A `simeq_bmi_transition_model` object.
#' @param donor_pool A validated historical entrant donor pool.
#' @param entrant_targets A validated entrant-target table for the next year.
#' @param n_entrants Integer number of microdata entrants to generate.
#' @param hayes_bmi_model Output from [simeq_fit_bmi_transition_hayes()].
#'   Required when `bmi_update_method = "historical"`.
#' @param bmi_update_method `"legacy"` (cohort-mean change), `"historical"`,
#'   or `"percentile"` (change depends on each person's rank within their
#'   sex, education and age group; see [simeq_update_bmi_percentile()]).
#' @param bmi_percentile_model Output from
#'   [simeq_fit_bmi_transition_percentile()]. Required when
#'   `bmi_update_method = "percentile"`.
#' @param min_cell_n Minimum NHS sample size in both interval endpoints for an
#'   empirical historical BMI transition.
#' @param stochastic_bmi Logical; add individual-level stochastic BMI variation.
#' @param education_source Either `"existing"` or `"map_3_to_2"`.
#' @param entry_age Integer entrant age.
#' @param max_age Integer maximum represented age.
#' @param bmi_limits Numeric lower/upper permitted BMI values.
#' @param mortality_calibration_rr Optional fixed stratum mean mortality
#'   relative risks from [simeq_mortality_calibration_rr()]. `NULL` recalibrates
#'   mortality to the life table every year.
#' @param bmi_period For `bmi_update_method = "legacy"` or `"percentile"`:
#'   `"latest"` (default) predicts every year with the latest fitted survey
#'   period (the projection period for `"percentile"`); `"by_year"`
#'   uses the most recent fitted period starting at or before the current
#'   year (for example the 1998 to 2005 period for 1998 to 2004), and the
#'   latest period from then on.
#'
#' @param education_upgrade Optional output from
#'   [simeq_fit_education_upgrade()]. When supplied, survivors with medium
#'   education can move to high education each year (after the BMI update).
#'   `NULL` (default) keeps education fixed from entry.
#'
#' @return A list with `living_state`, `deaths`, and `mortality_probabilities`.
#' @export
simeq_advance_one_year <- function(
    state,
    life_table,
    rr_bmi,
    rr_education,
    bmi_transition_model = NULL,
    donor_pool,
    entrant_targets,
    n_entrants,
    hayes_bmi_model = NULL,
    bmi_update_method = c("legacy", "historical", "percentile"),
    min_cell_n = 30L,
    stochastic_bmi = TRUE,
    education_source = c("existing", "map_3_to_2"),
    entry_age = 20L,
    max_age = 84L,
    bmi_limits = c(10, 80),
    mortality_calibration_rr = NULL,
    bmi_period = c("latest", "by_year"),
    bmi_percentile_model = NULL,
    education_upgrade = NULL
) {
  simeq_validate_living_state(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  education_source <- match.arg(education_source)
  bmi_period <- match.arg(bmi_period)
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

  if (
    identical(bmi_update_method, "percentile") &&
    is.null(bmi_percentile_model)
  ) {
    stop(
      "`bmi_percentile_model` must be supplied when ",
      "`bmi_update_method = 'percentile'`.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(bmi_limits) ||
    length(bmi_limits) != 2L ||
    any(!is.finite(bmi_limits)) ||
    bmi_limits[1L] >= bmi_limits[2L]
  ) {
    stop(
      "`bmi_limits` must contain two increasing finite values.",
      call. = FALSE
    )
  }

  state_year <- unique(as.integer(state$year))

  if (length(state_year) != 1L) {
    stop(
      "`state` must contain records from exactly one calendar year.",
      call. = FALSE
    )
  }

  simulation_id <- unique(as.integer(state$simulation))

  if (length(simulation_id) != 1L) {
    stop(
      "`state` must contain records from exactly one simulation.",
      call. = FALSE
    )
  }

  current_year <- state_year[1L]
  next_year <- as.integer(current_year + 1L)

  # Step 1: apply stochastic mortality using BMI at the start of the year.
  mortality_result <- simeq_update_mortality(
    state = state,
    life_table = life_table,
    rr_bmi = rr_bmi,
    rr_education = rr_education,
    education_source = education_source,
    entry_age = entry_age,
    max_age = max_age,
    calibration_rr = mortality_calibration_rr
  )

  survivors <- mortality_result$living_state
  deaths <- mortality_result$deaths

  if (nrow(deaths) > 0L) {
    deaths$year <- rep.int(next_year, nrow(deaths))
  }

  # Step 2: update BMI for survivors, using their current age and BMI.
  if (nrow(survivors) > 0L) {
    if (identical(bmi_update_method, "historical")) {
      survivors <- simeq_update_bmi_historical(
        population = survivors,
        year = current_year,
        hayes_bmi_model = hayes_bmi_model,
        min_cell_n = min_cell_n,
        stochastic = stochastic_bmi,
        min_bmi = bmi_limits[1L],
        max_bmi = bmi_limits[2L]
      )
    } else if (identical(bmi_update_method, "percentile")) {
      survivors <- simeq_update_bmi_percentile(
        state = survivors,
        bmi_percentile_model = bmi_percentile_model,
        period = .bmi_percentile_period_for_year(
          bmi_percentile_model = bmi_percentile_model,
          year = current_year,
          bmi_period = bmi_period
        ),
        bmi_limits = bmi_limits
      )
    } else {
      survivors <- simeq_update_bmi(
        state = survivors,
        bmi_transition_model = bmi_transition_model,
        bmi_limits = bmi_limits,
        period = .bmi_period_for_year(
          bmi_transition_model = bmi_transition_model,
          year = current_year,
          bmi_period = bmi_period
        )
      )
    }
  }

  # Step 2b: some survivors with medium education complete higher
  # education (ages set by `education_upgrade`).
  if (!is.null(education_upgrade) && nrow(survivors) > 0L) {
    survivors <- simeq_update_education(
      state = survivors,
      education_upgrade = education_upgrade
    )
  }

  # Step 3: age survivors and advance the calendar year.
  if (nrow(survivors) > 0L) {
    survivors <- simeq_update_demography(
      state = survivors,
      entry_age = entry_age,
      max_age = max_age
    )
  }

  entrants <- simeq_generate_entrants(
    donor_pool = donor_pool,
    entrant_targets = entrant_targets,
    entry_year = next_year,
    simulation = simulation_id[1L],
    n_entrants = n_entrants,
    entry_age = entry_age
  )

  living_state <- dplyr::bind_rows(
    survivors,
    entrants
  )

  simeq_validate_living_state(
    state = living_state,
    entry_age = entry_age,
    max_age = max_age
  )

  list(
    living_state = living_state,
    deaths = deaths,
    mortality_probabilities = mortality_result$mortality_probabilities
  )
}

# Survey period to predict BMI change with in `year`.
.bmi_period_for_year <- function(bmi_transition_model, year, bmi_period) {
  if (identical(bmi_period, "latest")) {
    return(NULL)
  }

  fitted_periods <- as.integer(
    bmi_transition_model$model$xlevels$period
  )
  eligible <- fitted_periods[fitted_periods <= as.integer(year)]

  if (length(eligible) == 0L) {
    return(min(fitted_periods))
  }

  max(eligible)
}

# The percentile model's projection period is used from the year it starts
# (by_year) or for every year (latest). Before the first fitted period the
# earliest period is used.
.bmi_percentile_period_for_year <- function(
    bmi_percentile_model,
    year,
    bmi_period
) {
  if (identical(bmi_period, "latest")) {
    return(bmi_percentile_model$projection_period)
  }

  eligible <- bmi_percentile_model$periods[
    bmi_percentile_model$periods <= as.integer(year)
  ]

  if (length(eligible) == 0L) {
    return(min(bmi_percentile_model$periods))
  }

  min(max(eligible), bmi_percentile_model$projection_period)
}
