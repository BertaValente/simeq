#' Initialise a SIMEQ population state
#'
#' Converts a validated baseline population into the standard person-level
#' state used by the SIMEQ simulation engine. Additional baseline
#' variables, such as region or an alternative education variable, are
#' retained in the returned state.
#'
#' @param population A validated baseline population data frame.
#' @param settings A `simeq_settings` object created by [simeq_settings()].
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
#' settings <- simeq_settings()
#'
#' simeq_initialise_population(
#'   population = population,
#'   settings = settings
#' )
#' Initialise the baseline simulated population
#'
#' @param population A validated baseline population data frame.
#' @param settings A `simeq_settings` object.
#'
#' @return A baseline simulation-state data frame.
#' @export
simeq_initialise_population <- function(
    population,
    settings
) {
  if (!inherits(settings, "simeq_settings")) {
    stop(
      "`settings` must be a `simeq_settings` object.",
      call. = FALSE
    )
  }

  simeq_validate_population(
    population = population,
    initial_year = settings$initial_year,
    entry_age = settings$entry_age,
    max_age = settings$max_age
  )

  education_source <- if (
    "education_level2" %in% names(population)
  ) {
    "existing"
  } else {
    "map_3_to_2"
  }

  population <- simeq_prepare_education_level2(
    state = population,
    source = education_source
  )

  core_state <- data.frame(
    person_id = as.character(population$person_id),
    simulation = rep.int(1L, nrow(population)),
    year = rep.int(settings$initial_year, nrow(population)),
    baseline_year = rep.int(settings$initial_year, nrow(population)),
    age = as.integer(population$age),
    sex = as.character(population$sex),
    education_level = as.character(population$education_level),
    education_level2 = as.character(population$education_level2),
    survey_weight = as.numeric(population$survey_weight),
    bmi = as.numeric(population$bmi),
    bmi_category = simeq_bmi_category(population$bmi),
    obesity = as.logical(population$bmi >= 30),
    alive = rep.int(TRUE, nrow(population)),
    death_year = rep.int(NA_integer_, nrow(population)),
    age_at_death = rep.int(NA_integer_, nrow(population)),
    stringsAsFactors = FALSE
  )

  additional_columns <- setdiff(
    names(population),
    names(core_state)
  )

  initial_state <- if (length(additional_columns) == 0L) {
    core_state
  } else {
    cbind(
      core_state,
      population[, additional_columns, drop = FALSE]
    )
  }

  if (!identical(
    as.numeric(initial_state$bmi),
    as.numeric(population$bmi)
  )) {
    stop(
      "Initialisation changed BMI values unexpectedly.",
      call. = FALSE
    )
  }

  if (!identical(
    as.numeric(initial_state$survey_weight),
    as.numeric(population$survey_weight)
  )) {
    stop(
      "Initialisation changed survey weights unexpectedly.",
      call. = FALSE
    )
  }

  initial_state
}
