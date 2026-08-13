#' Update the annual demographic state
#'
#' Advances a living PONTE-PT-O population state through its non-mortality
#' demographic update. Each person ages by one year, subject to the model's
#' upper age cap, and the simulation calendar advances by one year.
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
#' state <- ponte_initialise_population(
#'   population = population,
#'   settings = ponte_settings()
#' )
#'
#' ponte_update_demography(state)
ponte_update_demography <- function(
    state,
    entry_age = 20L,
    max_age = 84L
) {
  living_state <- ponte_age_population(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  ponte_advance_year(
    state = living_state,
    entry_age = entry_age,
    max_age = max_age
  )
}
