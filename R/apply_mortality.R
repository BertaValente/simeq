#' Apply annual mortality to a living population state
#'
#' Draws annual deaths from person-specific death probabilities and separates
#' deaths from surviving people. Deaths are removed from the active living
#' state and returned in a separate data frame.
#'
#' @param state A valid living PONTE-PT-O population state.
#' @param mortality_probabilities A data frame with `person_id`, `simulation`,
#'   and `p_die`, typically produced by
#'   `ponte_calculate_death_probability()`.
#' @param death_year Integer. Calendar year in which deaths are recorded.
#'   Defaults to one year after each record's current `year`.
#' @param entry_age Integer. Minimum represented age.
#' @param max_age Integer. Maximum represented age.
#'
#' @return A named list containing:
#' \describe{
#'   \item{living_state}{People who survive the annual mortality draw.}
#'   \item{deaths}{People who die during the annual interval.}
#' }
#' @export
#'
#' @examples
#' population <- data.frame(
#'   person_id = c("person_1", "person_2"),
#'   age = c(35L, 52L),
#'   sex = c("women", "men"),
#'   education_level = c("high", "medium"),
#'   bmi = c(23.5, 31.2),
#'   survey_weight = c(1000, 1200)
#' )
#'
#' state <- ponte_initialise_population(
#'   population = population,
#'   settings = ponte_settings()
#' )
#'
#' mortality_probabilities <- data.frame(
#'   person_id = c("person_1", "person_2"),
#'   simulation = c(1L, 1L),
#'   p_die = c(0, 1)
#' )
#'
#' ponte_apply_mortality(
#'   state = state,
#'   mortality_probabilities = mortality_probabilities
#' )
ponte_apply_mortality <- function(
    state,
    mortality_probabilities,
    death_year = NULL,
    entry_age = 20L,
    max_age = 84L
) {
  ponte_validate_living_state(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  if (!is.data.frame(mortality_probabilities)) {
    stop(
      "`mortality_probabilities` must be a data frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "person_id",
    "simulation",
    "p_die"
  )

  missing_columns <- setdiff(
    required_columns,
    names(mortality_probabilities)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`mortality_probabilities` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  p_die <- mortality_probabilities$p_die

  if (
    !is.numeric(p_die) ||
    anyNA(p_die) ||
    any(!is.finite(p_die)) ||
    any(p_die < 0 | p_die > 1)
  ) {
    stop(
      "`p_die` must contain finite probabilities from 0 to 1.",
      call. = FALSE
    )
  }

  probability_identifier <- paste(
    as.integer(mortality_probabilities$simulation),
    as.character(mortality_probabilities$person_id),
    sep = "::"
  )

  if (anyDuplicated(probability_identifier) > 0L) {
    stop(
      "`mortality_probabilities` must contain one record per ",
      "person-simulation combination.",
      call. = FALSE
    )
  }

  state_identifier <- paste(
    as.integer(state$simulation),
    as.character(state$person_id),
    sep = "::"
  )

  if (
    length(probability_identifier) != length(state_identifier) ||
    any(!state_identifier %in% probability_identifier) ||
    any(!probability_identifier %in% state_identifier)
  ) {
    stop(
      "`mortality_probabilities` must contain exactly one matching record ",
      "for every state record.",
      call. = FALSE
    )
  }

  probability_index <- match(
    state_identifier,
    probability_identifier
  )

  p_die <- p_die[
    probability_index
  ]

  if (is.null(death_year)) {
    death_year <- as.integer(
      state$year + 1L
    )
  } else {
    if (
      !is.numeric(death_year) ||
      length(death_year) != 1L ||
      is.na(death_year) ||
      !is.finite(death_year) ||
      death_year != as.integer(death_year)
    ) {
      stop(
        "`death_year` must be one finite integer value.",
        call. = FALSE
      )
    }

    death_year <- rep(
      as.integer(death_year),
      nrow(state)
    )
  }

  if (any(death_year <= state$year)) {
    stop(
      "`death_year` must be later than the current state `year`.",
      call. = FALSE
    )
  }

  died <- stats::rbinom(
    n = nrow(state),
    size = 1L,
    prob = p_die
  ) == 1L

  deaths <- state[
    died,
    ,
    drop = FALSE
  ]

  living_state <- state[
    !died,
    ,
    drop = FALSE
  ]

  if (nrow(deaths) > 0L) {
    deaths$alive <- FALSE

    deaths$death_year <- as.integer(
      death_year[died]
    )

    deaths$age_at_death <- as.integer(
      deaths$age
    )
  }

  list(
    living_state = living_state,
    deaths = deaths
  )
}
