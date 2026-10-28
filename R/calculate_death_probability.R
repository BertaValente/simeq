#' Calculate annual probability of death
#'
#' Calculates an individual annual probability of death for every record in a
#' living SIMEQ population state. The life-table hazard is recalibrated
#' within each age-sex stratum so that the population-weighted mean individual
#' hazard equals the life-table hazard for that stratum. Individual hazards are
#' then scaled by the product of the BMI and education relative risks.
#'
#' By default the stratum mean relative risk is taken from the current
#' population, so simulated deaths in each age-sex stratum always match the
#' life table and changes in BMI only change who dies, not how many. Supplying
#' `calibration_rr` (from [simeq_mortality_calibration_rr()] on a baseline
#' population) fixes the scaling instead, so changes in BMI and education
#' after the baseline year change the number of deaths.
#'
#' If `life_table` has a `year` column, the table for the state's year is
#' used (see [simeq_select_life_table_year()]).
#'
#' @param state A valid living SIMEQ population state.
#' @param life_table A validated life table with `age`, `sex`, and `qx`.
#' @param rr_bmi A validated BMI mortality relative-risk table.
#' @param rr_education A validated education mortality relative-risk table.
#' @param education_source Character string. Either `"existing"` when
#'   `state` already contains `education_level2`, or `"map_3_to_2"` to derive
#'   it from `education_level`.
#' @param entry_age Integer. Minimum represented age.
#' @param max_age Integer. Maximum represented age.
#' @param calibration_rr Optional data frame with columns `age`, `sex`, and
#'   `mean_rr`: fixed stratum mean relative risks. Strata missing from it use
#'   the current population's mean. Default `NULL` recalibrates every year.
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
#' simeq_calculate_death_probability(
#'   state = state,
#'   life_table = life_table,
#'   rr_bmi = rr_bmi,
#'   rr_education = rr_education,
#'   education_source = "map_3_to_2"
#' )
simeq_calculate_death_probability <- function(
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
  simeq_validate_living_state(
    state = state,
    entry_age = entry_age,
    max_age = max_age
  )

  simeq_validate_life_table(
    life_table = life_table,
    entry_age = entry_age,
    max_age = max_age
  )

  simeq_validate_mortality_risks(
    rr_bmi = rr_bmi,
    rr_education = rr_education
  )

  education_source <- match.arg(
    education_source
  )

  state <- simeq_prepare_education_level2(
    state = state,
    source = education_source
  )

  state_years <- unique(as.integer(state$year))

  if ("year" %in% names(life_table)) {
    if (length(state_years) != 1L) {
      stop(
        "`state` must contain one calendar year when `life_table` has a ",
        "`year` column.",
        call. = FALSE
      )
    }

    life_table <- simeq_select_life_table_year(
      life_table = life_table,
      year = state_years
    )
  }

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

  education_relative_risk <- .education_relative_risk(
    state = state,
    rr_education = rr_education
  )

  total_relative_risk <- bmi_relative_risk *
    education_relative_risk

  # Convert life-table probability to hazard
  h_lifetable <- -log(1 - qx)

  # Recalibrate baseline hazard within each age-sex stratum so that the
  # population-weighted mean individual hazard equals the life-table hazard.
  # This ensures that simulated mortality aggregates back to the observed
  # life-table mortality for each stratum, regardless of the simulated
  # population's BMI and education composition.
  stratum_id <- paste(
    as.integer(state$age),
    as.character(state$sex),
    sep = "::"
  )

  w <- state$survey_weight

  stratum_mean_rr <- tapply(w * total_relative_risk, stratum_id, sum) /
    tapply(w, stratum_id, sum)

  mean_rr_per_person <- as.numeric(stratum_mean_rr[stratum_id])

  if (!is.null(calibration_rr)) {
    if (
      !is.data.frame(calibration_rr) ||
      !all(c("age", "sex", "mean_rr") %in% names(calibration_rr))
    ) {
      stop(
        "`calibration_rr` must be a data frame with `age`, `sex`, and ",
        "`mean_rr`.",
        call. = FALSE
      )
    }

    fixed_rr <- calibration_rr$mean_rr[
      match(
        stratum_id,
        paste(
          as.integer(calibration_rr$age),
          as.character(calibration_rr$sex),
          sep = "::"
        )
      )
    ]

    mean_rr_per_person <- ifelse(
      is.na(fixed_rr),
      mean_rr_per_person,
      fixed_rr
    )
  }

  # Individual hazard: life-table hazard divided by stratum mean RR,
  # then scaled by the individual's own RR
  h_individual <- (h_lifetable / mean_rr_per_person) * total_relative_risk

  p_die <- 1 - exp(-h_individual)

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

#' Stratum mean mortality relative risks for fixed calibration
#'
#' Calculates the survey-weighted mean combined (BMI times education)
#' mortality relative risk in each age-sex stratum of a population. Pass the
#' result as `calibration_rr` to [simeq_calculate_death_probability()] to keep
#' the life-table calibration fixed at this population instead of
#' recalibrating every year.
#'
#' @inheritParams simeq_calculate_death_probability
#'
#' @return A data frame with `age`, `sex`, and `mean_rr`.
#' @export
simeq_mortality_calibration_rr <- function(
    state,
    rr_bmi,
    rr_education,
    education_source = c(
      "existing",
      "map_3_to_2"
    )
) {
  simeq_validate_mortality_risks(
    rr_bmi = rr_bmi,
    rr_education = rr_education
  )

  education_source <- match.arg(
    education_source
  )

  state <- simeq_prepare_education_level2(
    state = state,
    source = education_source
  )

  bmi_relative_risk <- rr_bmi$rr_bmi[
    match(
      paste(as.character(state$sex), as.character(state$bmi_category), sep = "::"),
      paste(as.character(rr_bmi$sex), as.character(rr_bmi$bmi_category), sep = "::")
    )
  ]

  education_relative_risk <- .education_relative_risk(
    state = state,
    rr_education = rr_education
  )

  if (anyNA(bmi_relative_risk) || anyNA(education_relative_risk)) {
    stop(
      "Missing mortality relative risks after matching state records.",
      call. = FALSE
    )
  }

  weighted <- data.frame(
    age = as.integer(state$age),
    sex = as.character(state$sex),
    weighted_rr = state$survey_weight * bmi_relative_risk *
      education_relative_risk,
    survey_weight = state$survey_weight,
    stringsAsFactors = FALSE
  )

  calibration <- stats::aggregate(
    cbind(weighted_rr, survey_weight) ~ age + sex,
    data = weighted,
    FUN = sum
  )

  data.frame(
    age = calibration$age,
    sex = calibration$sex,
    mean_rr = calibration$weighted_rr / calibration$survey_weight,
    stringsAsFactors = FALSE
  )
}

#' Select the life table for a calendar year
#'
#' Returns the `age`, `sex`, `qx` table to use for `year`. If `life_table` has
#' no `year` column it is returned unchanged. Otherwise the most recent table
#' at or before `year` is used; years before the first table use the first
#' table, and years after the last table carry the last table forward.
#'
#' @param life_table A life table, optionally with a `year` column.
#' @param year One calendar year.
#'
#' @return A life table without a `year` column.
#' @export
simeq_select_life_table_year <- function(
    life_table,
    year
) {
  if (!"year" %in% names(life_table)) {
    return(life_table)
  }

  if (length(year) != 1L || is.na(year)) {
    stop("`year` must be one calendar year.", call. = FALSE)
  }

  table_years <- sort(unique(as.integer(life_table$year)))
  eligible_years <- table_years[table_years <= as.integer(year)]
  chosen_year <- if (length(eligible_years) > 0L) {
    max(eligible_years)
  } else {
    min(table_years)
  }

  selected <- life_table[
    as.integer(life_table$year) == chosen_year,
    setdiff(names(life_table), "year"),
    drop = FALSE
  ]

  rownames(selected) <- NULL
  selected
}

# Education mortality relative risk for each person: by education group, or
# `rr_per_year ^ education_years` when `rr_education` has `rr_per_year`
# (age-specific when it has `age_min` and `age_max`).
.education_relative_risk <- function(state, rr_education) {
  if (!"rr_per_year" %in% names(rr_education)) {
    education_relative_risk <- rr_education$rr_education[
      match(
        as.character(state$education_level2),
        as.character(rr_education$education_level2)
      )
    ]

    if (anyNA(education_relative_risk)) {
      stop(
        "Missing education mortality relative risks after matching ",
        "state records.",
        call. = FALSE
      )
    }

    return(education_relative_risk)
  }

  if (!"education_years" %in% names(state)) {
    stop(
      "`state` must contain `education_years` when `rr_education` gives ",
      "`rr_per_year`.",
      call. = FALSE
    )
  }

  education_years <- as.numeric(state$education_years)

  if (anyNA(education_years) || any(education_years < 0)) {
    stop(
      "`education_years` must be non-missing and non-negative.",
      call. = FALSE
    )
  }

  if (all(c("age_min", "age_max") %in% names(rr_education))) {
    row_index <- vapply(
      as.integer(state$age),
      function(age) {
        matched <- which(
          age >= rr_education$age_min & age <= rr_education$age_max
        )
        if (length(matched) == 0L) NA_integer_ else matched[1L]
      },
      integer(1)
    )

    if (anyNA(row_index)) {
      stop(
        "`rr_education` age ranges do not cover every age in `state`.",
        call. = FALSE
      )
    }

    rr_per_year <- rr_education$rr_per_year[row_index]
  } else {
    rr_per_year <- rep(rr_education$rr_per_year[1L], nrow(state))
  }

  rr_per_year^education_years
}
