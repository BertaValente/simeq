#' Validate a living PONTE-PT-O population state
#'
#' Checks whether a person-level population state is valid for an annual
#' PONTE-PT-O simulation update. All records must represent people who are
#' alive at the start of the update.
#'
#' @param state A data frame containing a living PONTE-PT-O population state.
#' @param entry_age Integer. Minimum eligible age.
#' @param max_age Integer. Maximum represented age.
#'
#' @return The input `state`, invisibly, if all checks pass.
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
#' simeq_validate_living_state(state)
simeq_validate_living_state <- function(
    state,
    entry_age = 20L,
    max_age = 84L
) {
  if (!is.data.frame(state)) {
    stop(
      "`state` must be a data frame.",
      call. = FALSE
    )
  }

  if (nrow(state) == 0L) {
    stop(
      "`state` must contain at least one record.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "person_id",
    "simulation",
    "year",
    "baseline_year",
    "age",
    "sex",
    "education_level",
    "survey_weight",
    "bmi",
    "bmi_category",
    "obesity",
    "alive",
    "death_year",
    "age_at_death"
  )

  missing_columns <- setdiff(
    required_columns,
    names(state)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`state` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  simulation <- state$simulation

  if (
    !is.numeric(simulation) ||
    anyNA(simulation) ||
    any(!is.finite(simulation)) ||
    any(simulation != as.integer(simulation)) ||
    any(simulation < 1L)
  ) {
    stop(
      "`simulation` must contain positive, non-missing integer values.",
      call. = FALSE
    )
  }

  state_identifier <- paste(
    as.integer(simulation),
    as.character(state$person_id),
    sep = "::"
  )

  if (anyDuplicated(state_identifier) > 0L) {
    stop(
      "Each `person_id` must be unique within each simulation.",
      call. = FALSE
    )
  }

  state_for_population_validation <- state

  state_for_population_validation$person_id <- state_identifier

  simeq_validate_population(
    population = state_for_population_validation,
    initial_year = min(state$year),
    entry_age = entry_age,
    max_age = max_age
  )

  year <- state$year

  if (
    !is.numeric(year) ||
    anyNA(year) ||
    any(!is.finite(year)) ||
    any(year != as.integer(year))
  ) {
    stop(
      "`year` must contain finite, non-missing integer values.",
      call. = FALSE
    )
  }

  baseline_year <- state$baseline_year

  if (
    !is.numeric(baseline_year) ||
    anyNA(baseline_year) ||
    any(!is.finite(baseline_year)) ||
    any(baseline_year != as.integer(baseline_year))
  ) {
    stop(
      "`baseline_year` must contain finite, non-missing integer values.",
      call. = FALSE
    )
  }

  if (any(year < baseline_year)) {
    stop(
      "`year` must not be earlier than `baseline_year`.",
      call. = FALSE
    )
  }

  bmi_category <- as.character(
    state$bmi_category
  )

  expected_bmi_category <- simeq_bmi_category(
    state$bmi
  )

  if (
    anyNA(bmi_category) ||
    !identical(
      bmi_category,
      expected_bmi_category
    )
  ) {
    stop(
      "`bmi_category` must match the BMI values.",
      call. = FALSE
    )
  }

  obesity <- state$obesity

  if (
    !is.logical(obesity) ||
    anyNA(obesity) ||
    !identical(
      obesity,
      state$bmi >= 30
    )
  ) {
    stop(
      "`obesity` must be logical and match the BMI values.",
      call. = FALSE
    )
  }

  if (!is.logical(state$alive) || anyNA(state$alive)) {
    stop(
      "`alive` must contain non-missing logical values.",
      call. = FALSE
    )
  }

  if (!all(state$alive)) {
    stop(
      "`state` must contain only living records.",
      call. = FALSE
    )
  }

  if (any(!is.na(state$death_year))) {
    stop(
      "`death_year` must be missing for living records.",
      call. = FALSE
    )
  }

  if (any(!is.na(state$age_at_death))) {
    stop(
      "`age_at_death` must be missing for living records.",
      call. = FALSE
    )
  }

  invisible(state)
}
