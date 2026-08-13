#' Advance a living PONTE-PT-O state by one calendar year
#'
#' Advances the simulation calendar by one year. This function does not yet
#' change age, BMI, mortality, or population membership.
#'
#' @param state A valid living PONTE-PT-O population state.
#' @param entry_age Integer. Minimum eligible age.
#' @param max_age Integer. Maximum represented age.
#'
#' @return A data frame with `year` increased by one for every record.
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
#' ponte_advance_year(state)
ponte_advance_year <- function(
    state,
    entry_age = 20L,
    max_age = 84L
) {
  ponte_validate_living_state(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  state$year <- as.integer(
    state$year + 1L
  )

  state
}
