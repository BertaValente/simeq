#' Calculate years of life lost
#'
#' Calculates individual and weighted years of life lost (YLL) from
#' PONTE-PT-O microsimulation output.
#'
#' YLL is calculated using a reference life expectancy:
#'
#' \deqn{
#'   YLL = \max(\text{ref_age} - \text{age_at_death}, 0)
#' }
#'
#' Alternatively, supply `ref_life_expectancy` to use the remaining life
#' expectancy at the age of death, as in the Global Burden of Disease study:
#' YLL is then the `ex` value for the person's sex and age at death, so deaths
#' above `ref_age` still count.
#'
#' @param simulation_output A data frame returned by
#'   [simeq_run_simulation()] containing death-history fields.
#' @param ref_age Reference life expectancy for YLL calculation. Either a
#'   single numeric scalar applied to all individuals, or a named numeric
#'   vector with elements `"men"` and `"women"` for sex-specific reference
#'   ages (e.g. `c(men = 77L, women = 81L)`).
#' @param min_age Integer minimum age at death included in YLL calculations.
#' @param max_age Integer maximum age at death included in YLL calculations.
#' @param ref_life_expectancy Optional data frame with columns `age`, `sex`
#'   (`"men"`, `"women"`), and `ex` (remaining life expectancy at that age),
#'   covering every age from `min_age` to `max_age`. When supplied, it
#'   replaces `ref_age`.
#'
#' @return A named list containing:
#' \describe{
#'   \item{individual}{One row per person and simulation with YLL values.}
#'   \item{overall_sim}{Weighted YLL summaries by simulation, sex, and
#'     education level.}
#'   \item{overall}{Mean and 95% simulation interval summaries by sex and
#'     education level.}
#'   \item{by_year_sim}{Weighted YLL summaries by death year and simulation.}
#'   \item{by_year}{Mean annual weighted YLL summaries across simulations.}
#' }
#'
#' @export
simeq_calculate_yll <- function(
    simulation_output,
    ref_age = 75L,
    min_age = 20L,
    max_age = 84L,
    ref_life_expectancy = NULL
) {
  if (!is.data.frame(simulation_output)) {
    stop(
      "`simulation_output` must be a data frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "person_id",
    "simulation",
    "sex",
    "education_level",
    "survey_weight",
    "death_year",
    "age_at_death"
  )

  missing_columns <- setdiff(
    required_columns,
    names(simulation_output)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (length(ref_age) == 1L) {
    if (!is.numeric(ref_age) || !is.finite(ref_age)) {
      stop("`ref_age` must be a finite numeric value.", call. = FALSE)
    }
    ref_age <- c(men = as.numeric(ref_age), women = as.numeric(ref_age))
  } else {
    if (
      !identical(sort(names(ref_age)), c("men", "women")) ||
      !is.numeric(ref_age) ||
      any(!is.finite(ref_age))
    ) {
      stop(
        '`ref_age` must be a named numeric vector with elements "men" and "women".',
        call. = FALSE
      )
    }
  }

  if (
    length(min_age) != 1L ||
    length(max_age) != 1L ||
    !is.numeric(min_age) ||
    !is.numeric(max_age) ||
    !is.finite(min_age) ||
    !is.finite(max_age) ||
    min_age > max_age
  ) {
    stop(
      "`min_age` and `max_age` must be finite values with ",
      "`min_age <= max_age`.",
      call. = FALSE
    )
  }

  if (
    any(!is.finite(simulation_output$survey_weight)) ||
    any(simulation_output$survey_weight < 0)
  ) {
    stop(
      "`survey_weight` must contain finite non-negative values.",
      call. = FALSE
    )
  }

  if (nrow(simulation_output) == 0L) {
    empty_individual <- tibble::tibble(
      person_id = character(),
      simulation = integer(),
      sex = character(),
      education_level = character(),
      survey_weight = numeric(),
      death_year = integer(),
      age_at_death = integer(),
      died = logical(),
      yll = numeric(),
      weighted_yll = numeric(),
      weighted_death = numeric()
    )

    empty_overall_sim <- tibble::tibble(
      simulation = integer(),
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric(),
      total_population_weighted = numeric(),
      mean_yll_per_death = numeric(),
      mean_yll_per_person = numeric()
    )

    empty_overall <- tibble::tibble(
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric(),
      total_population_weighted = numeric(),
      mean_yll_per_death = numeric(),
      mean_yll_per_person = numeric(),
      yll_lo = numeric(),
      yll_hi = numeric()
    )

    empty_by_year_sim <- tibble::tibble(
      simulation = integer(),
      death_year = integer(),
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric()
    )

    empty_by_year <- tibble::tibble(
      death_year = integer(),
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric()
    )

    return(
      list(
        individual = empty_individual,
        overall_sim = empty_overall_sim,
        overall = empty_overall,
        by_year_sim = empty_by_year_sim,
        by_year = empty_by_year
      )
    )
  }

  individual <- simulation_output |>
    dplyr::group_by(
      person_id,
      simulation,
      sex,
      education_level,
      survey_weight
    ) |>
    dplyr::summarise(
      death_year   = dplyr::first(death_year[!is.na(death_year)]),
      age_at_death = dplyr::first(age_at_death[!is.na(age_at_death)]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      simulation   = as.integer(simulation),
      sex          = as.character(sex),
      education_level = as.character(education_level),
      survey_weight   = as.numeric(survey_weight),
      death_year      = as.integer(death_year),
      age_at_death    = as.integer(age_at_death),
      died = !is.na(death_year) & !is.na(age_at_death),
      years_remaining = .yll_years_remaining(
        sex = sex,
        age_at_death = age_at_death,
        ref_age = ref_age,
        ref_life_expectancy = ref_life_expectancy
      ),
      yll  = dplyr::if_else(
        died &
          age_at_death >= min_age &
          age_at_death <= max_age,
        years_remaining,
        0
      ),
      years_remaining = NULL,
      weighted_yll   = yll * survey_weight,
      weighted_death = dplyr::if_else(died, survey_weight, 0)
    )

  overall_sim <- individual |>
    dplyr::group_by(
      simulation,
      sex,
      education_level
    ) |>
    dplyr::summarise(
      total_yll_weighted = sum(
        weighted_yll,
        na.rm = TRUE
      ),
      total_deaths_weighted = sum(
        weighted_death,
        na.rm = TRUE
      ),
      total_population_weighted = sum(
        survey_weight,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean_yll_per_death = dplyr::if_else(
        total_deaths_weighted > 0,
        total_yll_weighted / total_deaths_weighted,
        NA_real_
      ),
      mean_yll_per_person = dplyr::if_else(
        total_population_weighted > 0,
        total_yll_weighted / total_population_weighted,
        NA_real_
      )
    )

  overall <- overall_sim |>
    dplyr::group_by(
      sex,
      education_level
    ) |>
    dplyr::summarise(
      total_yll_weighted = mean(
        total_yll_weighted,
        na.rm = TRUE
      ),
      total_deaths_weighted = mean(
        total_deaths_weighted,
        na.rm = TRUE
      ),
      total_population_weighted = mean(
        total_population_weighted,
        na.rm = TRUE
      ),
      mean_yll_per_death = mean(
        mean_yll_per_death,
        na.rm = TRUE
      ),
      mean_yll_per_person = mean(
        mean_yll_per_person,
        na.rm = TRUE
      ),
      yll_lo = stats::quantile(
        mean_yll_per_person,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
      yll_hi = stats::quantile(
        mean_yll_per_person,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
      .groups = "drop"
    )

  deaths <- individual |>
    dplyr::filter(
      died,
      !is.na(death_year)
    )

  if (nrow(deaths) == 0L) {
    by_year_sim <- tibble::tibble(
      simulation = integer(),
      death_year = integer(),
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric()
    )

    by_year <- tibble::tibble(
      death_year = integer(),
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric()
    )
  } else {
    by_year_sim <- deaths |>
      dplyr::group_by(
        simulation,
        death_year,
        sex,
        education_level
      ) |>
      dplyr::summarise(
        total_yll_weighted = sum(
          weighted_yll,
          na.rm = TRUE
        ),
        total_deaths_weighted = sum(
          weighted_death,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    by_year <- by_year_sim |>
      dplyr::group_by(
        death_year,
        sex,
        education_level
      ) |>
      dplyr::summarise(
        total_yll_weighted = mean(
          total_yll_weighted,
          na.rm = TRUE
        ),
        total_deaths_weighted = mean(
          total_deaths_weighted,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }

  list(
    individual = individual,
    overall_sim = overall_sim,
    overall = overall,
    by_year_sim = by_year_sim,
    by_year = by_year
  )
}

# Years of life lost per death: fixed reference age, or remaining life
# expectancy at the age of death when a table is supplied.
.yll_years_remaining <- function(sex, age_at_death, ref_age,
                                 ref_life_expectancy) {
  if (is.null(ref_life_expectancy)) {
    return(pmax(ref_age[sex] - as.numeric(age_at_death), 0))
  }

  if (
    !is.data.frame(ref_life_expectancy) ||
    !all(c("age", "sex", "ex") %in% names(ref_life_expectancy))
  ) {
    stop(
      "`ref_life_expectancy` must be a data frame with `age`, `sex`, and `ex`.",
      call. = FALSE
    )
  }

  ex <- ref_life_expectancy$ex[
    match(
      paste(as.integer(age_at_death), sex, sep = "::"),
      paste(
        as.integer(ref_life_expectancy$age),
        as.character(ref_life_expectancy$sex),
        sep = "::"
      )
    )
  ]

  missing_ex <- !is.na(age_at_death) & is.na(ex)

  if (any(missing_ex)) {
    stop(
      "`ref_life_expectancy` has no `ex` for some ages at death.",
      call. = FALSE
    )
  }

  pmax(as.numeric(ex), 0)
}
