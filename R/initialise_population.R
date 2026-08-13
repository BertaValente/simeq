#' Initialise a PONTE-PT-O population state
#'
#' Converts a validated baseline population into the standard person-level
#' state used by the PONTE-PT-O simulation engine. Additional baseline
#' variables, such as region or an alternative education variable, are
#' retained in the returned state.
#'
#' @param population A validated baseline population data frame.
#' @param settings A `ponte_settings` object created by [ponte_settings()].
#'
#' @return A data frame containing the initial state of each simulated person.
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
#' settings <- ponte_settings()
#'
#' ponte_initialise_population(
#'   population = population,
#'   settings = settings
#' )
ponte_initialise_population <- function(
    population,
    settings
) {
  if (!inherits(settings, "ponte_settings")) {
    stop(
      "`settings` must be a `ponte_settings` object.",
      call. = FALSE
    )
  }

  ponte_validate_population(
    population = population,
    initial_year = settings$initial_year,
    entry_age = settings$entry_age,
    max_age = settings$max_age
  )

  if (
    "education_level2" %in%
    names(population)
  ) {
    population <- ponte_prepare_education_level2(
      state = population,
      source = "existing"
    )
  } else {
    population <- ponte_prepare_education_level2(
      state = population,
      source = "map_3_to_2"
    )
  }

  core_state <- data.frame(
    person_id = as.character(population$person_id),
    simulation = rep.int(
      1L,
      nrow(population)
    ),
    year = rep.int(
      settings$initial_year,
      nrow(population)
    ),
    baseline_year = rep.int(
      settings$initial_year,
      nrow(population)
    ),
    age = as.integer(population$age),
    sex = as.character(population$sex),
    education_level = as.character(
      population$education_level
    ),
    education_level2 = as.character(
      population$education_level2
    ),
    survey_weight = as.numeric(
      population$survey_weight
    ),
    bmi = as.numeric(population$bmi),
    bmi_category = ponte_bmi_category(
      population$bmi
    ),
    obesity = as.logical(
      population$bmi >= 30
    ),
    alive = rep.int(
      TRUE,
      nrow(population)
    ),
    death_year = rep.int(
      NA_integer_,
      nrow(population)
    ),
    age_at_death = rep.int(
      NA_integer_,
      nrow(population)
    ),
    stringsAsFactors = FALSE
  )

  additional_columns <- setdiff(
    names(population),
    names(core_state)
  )

  if (length(additional_columns) == 0L) {
    return(core_state)
  }

  cbind(
    core_state,
    population[, additional_columns, drop = FALSE]
  )
}
