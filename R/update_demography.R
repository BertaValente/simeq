#' Update the annual demographic state
#'
#' Advances a living PONTE-PT-O population state through its non-mortality
#' demographic update. Each person ages by one year, people passing `max_age`
#' leave the modelled population, and the simulation calendar advances by one
#' year.
#'
#' @param state A valid living PONTE-PT-O population state.
#' @param entry_age Integer. Minimum supported age.
#' @param max_age Integer. Maximum represented age.
#'
#' @return An updated living population state.
#' @export
#'
#' @examples
#' population <- data.frame(
#'   person_id = c("person_1", "person_2"),
#'   age = c(35L, 84L),
#'   sex = c("women", "men"),
#'   education_level = c("high", "medium"),
#'   bmi = c(23.5, 31.2),
#'   survey_weight = c(1000, 1200)
#' )
#'
#' state <- simeq_initialise_population(
#'   population = population,
#'   settings = simeq_settings()
#' )
#'
#' simeq_update_demography(state)
simeq_update_demography <- function(
    state,
    entry_age = 20L,
    max_age = 84L
) {
  living_state <- simeq_age_population(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  simeq_advance_year(
    state = living_state,
    entry_age = entry_age,
    max_age = max_age
  )
}
