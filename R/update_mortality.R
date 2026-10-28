#' Update annual mortality in a living population state
#'
#' Calculates life-table-based annual death probabilities, adjusts them for
#' BMI and education relative risks, then draws deaths and separates survivors
#' from death records.
#'
#' @param state A valid living SIMEQ population state.
#' @param life_table A validated life table with `age`, `sex`, and `qx`.
#' @param rr_bmi A validated BMI mortality relative-risk table.
#' @param rr_education A validated education mortality relative-risk table.
#' @param education_source Character string. Either `"existing"` when
#'   `state` contains `education_level2`, or `"map_3_to_2"` to derive it from
#'   `education_level`.
#' @param entry_age Integer. Minimum represented age.
#' @param max_age Integer. Maximum represented age.
#' @param calibration_rr Optional fixed stratum mean relative risks; see
#'   [simeq_calculate_death_probability()].
#'
#' @return A named list containing:
#' \describe{
#'   \item{living_state}{People who survive the annual mortality draw.}
#'   \item{deaths}{People who die during the annual interval.}
#'   \item{mortality_probabilities}{Individual annual death probabilities used
#'   in the draw.}
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
#' state <- simeq_initialise_population(
#'   population = population,
#'   settings = simeq_settings()
#' )
#'
#' life_table <- expand.grid(
#'   age = 20:84,
#'   sex = c("men", "women"),
#'   stringsAsFactors = FALSE
#' )
#'
#' life_table$qx <- 0.01
#'
#' rr_bmi <- expand.grid(
#'   sex = c("men", "women"),
#'   bmi_category = c(
#'     "underweight",
#'     "normal_weight",
#'     "overweight",
#'     "obesity_I",
#'     "obesity_II",
#'     "obesity_III"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' rr_bmi$rr_bmi <- 1
#'
#' rr_education <- data.frame(
#'   education_level2 = c("lower", "higher"),
#'   rr_education = c(1, 1)
#' )
#'
#' simeq_update_mortality(
#'   state = state,
#'   life_table = life_table,
#'   rr_bmi = rr_bmi,
#'   rr_education = rr_education,
#'   education_source = "map_3_to_2"
#' )
simeq_update_mortality <- function(
    state,
    life_table,
    rr_bmi,
    rr_education,
    education_source = c(
      "existing",
      "map_3_to_2"
    ),
    entry_age = 20L,
    max_age = 84L,
    calibration_rr = NULL
) {
  education_source <- match.arg(
    education_source
  )

  mortality_probabilities <-
    simeq_calculate_death_probability(
      state = state,
      life_table = life_table,
      rr_bmi = rr_bmi,
      rr_education = rr_education,
      education_source = education_source,
      entry_age = entry_age,
      max_age = max_age,
      calibration_rr = calibration_rr
    )

  mortality_result <- simeq_apply_mortality(
    state = state,
    mortality_probabilities = mortality_probabilities,
    entry_age = entry_age,
    max_age = max_age
  )

  list(
    living_state = mortality_result$living_state,
    deaths = mortality_result$deaths,
    mortality_probabilities = mortality_probabilities
  )
}
