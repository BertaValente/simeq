#' Calculate annual probability of death
#'
#' Calculates an individual annual probability of death for every record in a
#' living PONTE-PT-O population state. Life-table probabilities are adjusted
#' by BMI-category and education relative risks on the hazard scale.
#'
#' @param state A valid living PONTE-PT-O population state.
#' @param life_table A validated life table with `age`, `sex`, and `qx`.
#' @param rr_bmi A validated BMI mortality relative-risk table.
#' @param rr_education A validated education mortality relative-risk table.
#' @param education_source Character string. Either `"existing"` when
#'   `state` already contains `education_level2`, or `"map_3_to_2"` to derive
#'   it from `education_level`.
#' @param entry_age Integer. Minimum represented age.
#' @param max_age Integer. Maximum represented age.
#'
#' @return A data frame with `person_id`, `simulation`, and `p_die`.
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
#'     "obesity"
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
#' ponte_calculate_death_probability(
#'   state = state,
#'   life_table = life_table,
#'   rr_bmi = rr_bmi,
#'   rr_education = rr_education,
#'   education_source = "map_3_to_2"
#' )
ponte_calculate_death_probability <- function(
    state,
    life_table,
    rr_bmi,
    rr_education,
    education_source = c(
      "existing",
      "map_3_to_2"
    ),
    entry_age = 20L,
    max_age = 84L
) {
  ponte_validate_living_state(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  ponte_validate_life_table(
    life_table = life_table,
    entry_age = entry_age,
    max_age = max_age
  )

  ponte_validate_mortality_risks(
    rr_bmi = rr_bmi,
    rr_education = rr_education
  )

  education_source <- match.arg(
    education_source
  )

  state <- ponte_prepare_education_level2(
    state = state,
    source = education_source
  )

  life_table_identifier <- paste(
    as.integer(life_table$age),
    as.character(life_table$sex),
    sep = "::"
  )

  state_life_table_identifier <- paste(
    as.integer(state$age),
    as.character(state$sex),
    sep = "::"
  )

  life_table_index <- match(
    state_life_table_identifier,
    life_table_identifier
  )

  qx <- life_table$qx[
    life_table_index
  ]

  if (anyNA(qx)) {
    stop(
      "Missing life-table `qx` values after matching state records.",
      call. = FALSE
    )
  }

  rr_bmi_identifier <- paste(
    as.character(rr_bmi$sex),
    as.character(rr_bmi$bmi_category),
    sep = "::"
  )

  state_rr_bmi_identifier <- paste(
    as.character(state$sex),
    as.character(state$bmi_category),
    sep = "::"
  )

  rr_bmi_index <- match(
    state_rr_bmi_identifier,
    rr_bmi_identifier
  )

  bmi_relative_risk <- rr_bmi$rr_bmi[
    rr_bmi_index
  ]

  if (anyNA(bmi_relative_risk)) {
    stop(
      "Missing BMI mortality relative risks after matching state records.",
      call. = FALSE
    )
  }

  rr_education_index <- match(
    as.character(state$education_level2),
    as.character(rr_education$education_level2)
  )

  education_relative_risk <- rr_education$rr_education[
    rr_education_index
  ]

  if (anyNA(education_relative_risk)) {
    stop(
      "Missing education mortality relative risks after matching ",
      "state records.",
      call. = FALSE
    )
  }

  total_relative_risk <- bmi_relative_risk *
    education_relative_risk

  p_die <- 1 - (
    1 - qx
  )^total_relative_risk

  p_die <- pmin(
    pmax(
      p_die,
      0
    ),
    1
  )

  data.frame(
    person_id = state$person_id,
    simulation = as.integer(
      state$simulation
    ),
    p_die = as.numeric(
      p_die
    ),
    stringsAsFactors = FALSE
  )
}
