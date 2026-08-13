#' Validate a PONTE-PT-O baseline population
#'
#' Checks whether a baseline population contains the minimum variables
#' and valid values required to initialise a PONTE-PT-O simulation.
#'
#' @param population A data frame with one row per person at baseline.
#' @param initial_year Integer. The year represented by the baseline
#'   population.
#' @param entry_age Integer. Minimum eligible age.
#' @param max_age Integer. Maximum eligible age.
#'
#' @return The input `population`, invisibly, if all checks pass.
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
#' ponte_validate_population(
#'   population = population,
#'   initial_year = 2020L
#' )
ponte_validate_population <- function(
    population,
    initial_year,
    entry_age = 20L,
    max_age = 84L
) {
  if (!is.data.frame(population)) {
    stop(
      "`population` must be a data frame.",
      call. = FALSE
    )
  }

  if (nrow(population) == 0L) {
    stop(
      "`population` must contain at least one record.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "person_id",
    "age",
    "sex",
    "education_level",
    "bmi",
    "survey_weight"
  )

  missing_columns <- setdiff(
    required_columns,
    names(population)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`population` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  integer_arguments <- list(
    initial_year = initial_year,
    entry_age = entry_age,
    max_age = max_age
  )

  invalid_integer_arguments <- names(integer_arguments)[
    !vapply(
      integer_arguments,
      function(x) {
        length(x) == 1L &&
          !is.na(x) &&
          is.finite(x) &&
          x == as.integer(x)
      },
      logical(1)
    )
  ]

  if (length(invalid_integer_arguments) > 0L) {
    stop(
      "The following arguments must be single finite integers: ",
      paste(invalid_integer_arguments, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  initial_year <- as.integer(initial_year)
  entry_age <- as.integer(entry_age)
  max_age <- as.integer(max_age)

  if (entry_age < 0L) {
    stop(
      "`entry_age` must be zero or greater.",
      call. = FALSE
    )
  }

  if (max_age <= entry_age) {
    stop(
      "`max_age` must be greater than `entry_age`.",
      call. = FALSE
    )
  }

  person_id <- as.character(
    population$person_id
  )

  if (anyNA(person_id) || any(!nzchar(person_id))) {
    stop(
      "`person_id` must not contain missing or empty values.",
      call. = FALSE
    )
  }

  if (anyDuplicated(person_id) > 0L) {
    stop(
      "`person_id` must be unique in the baseline population.",
      call. = FALSE
    )
  }

  age <- population$age

  if (
    !is.numeric(age) ||
    anyNA(age) ||
    any(!is.finite(age)) ||
    any(age != as.integer(age))
  ) {
    stop(
      "`age` must contain finite, non-missing integer values.",
      call. = FALSE
    )
  }

  age <- as.integer(age)

  if (any(age < entry_age | age > max_age)) {
    stop(
      "`age` must be between `entry_age` and `max_age`.",
      call. = FALSE
    )
  }

  sex <- as.character(
    population$sex
  )

  if (anyNA(sex) || any(!nzchar(sex))) {
    stop(
      "`sex` must not contain missing or empty values.",
      call. = FALSE
    )
  }

  education_level <- as.character(
    population$education_level
  )

  if (
    anyNA(education_level) ||
    any(!nzchar(education_level))
  ) {
    stop(
      "`education_level` must not contain missing or empty values.",
      call. = FALSE
    )
  }

  bmi <- population$bmi

  if (
    !is.numeric(bmi) ||
    anyNA(bmi) ||
    any(!is.finite(bmi))
  ) {
    stop(
      "`bmi` must contain finite, non-missing numeric values.",
      call. = FALSE
    )
  }

  if (any(bmi < 10 | bmi > 80)) {
    stop(
      "`bmi` values must be between 10 and 80 kg/m^2.",
      call. = FALSE
    )
  }

  survey_weight <- population$survey_weight

  if (
    !is.numeric(survey_weight) ||
    anyNA(survey_weight) ||
    any(!is.finite(survey_weight)) ||
    any(survey_weight <= 0)
  ) {
    stop(
      "`survey_weight` must contain finite, positive numeric values.",
      call. = FALSE
    )
  }

  invisible(population)
}
