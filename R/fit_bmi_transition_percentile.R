#' Fit percentile-specific synthetic-cohort BMI transitions
#'
#' Extends the synthetic-cohort approach of [simeq_fit_bmi_transition()] from
#' the cohort mean to the cohort BMI distribution. For each pair of
#' consecutive survey waves, a synthetic cohort is defined by sex, two-level
#' education and five-year age group at the earlier wave, and is followed to
#' the same birth cohort at the later wave (ages shifted by the interval).
#' Survey-weighted BMI percentiles are computed at both waves, and the annual
#' change in each percentile is
#' `(percentile at later wave - percentile at earlier wave) / interval`.
#'
#' For every percentile level, the annual change is regressed on the same
#' predictors as the mean model (sex, age, age squared, education and survey
#' period), weighted by the geometric mean of the two waves' weighted cohort
#' sizes. In the simulation, [simeq_update_bmi_percentile()] gives each person
#' the change predicted at their rank within their sex, education and age
#' group, so the upper part of the distribution can rise faster than the
#' median, as observed in the survey waves.
#'
#' The later wave contains only the cohort members who survived the
#' interval, and people with high BMI die sooner, so the raw change between
#' waves mixes BMI change with survivor selection. The simulation applies
#' BMI-specific mortality separately, so using the raw change would count
#' that selection twice. When `life_table`, `rr_bmi` and `rr_education` are
#' supplied, each earlier-wave respondent's weight is multiplied by their
#' probability of surviving the interval, computed with the same mortality
#' model as the simulation (life-table hazard rescaled within sex and
#' five-year age group, times the BMI and education relative risks, with BMI
#' held at its earlier-wave value). The baseline percentiles then describe
#' the expected survivors, and the fitted change is the change among people
#' who stay alive.
#'
#' @param bmi_panel Repeated cross-sectional BMI data with `year`, `sex`,
#'   `age`, `education_level2`, `survey_weight` and `bmi`.
#' @param probs Percentile levels to model, strictly increasing within (0, 1).
#' @param min_n Minimum number of respondents required in a cohort at both
#'   waves.
#' @param projection_period Survey period (earlier wave year) used for years
#'   after the last fitted period starts. Defaults to the latest period.
#' @param max_age Maximum age at the later wave.
#' @param sex_interactions If `TRUE`, the age, age-squared and survey-period
#'   terms are allowed to differ by sex (`sex * (age + age^2 + period) +
#'   education`). The default `FALSE` uses additive terms only.
#' @param life_table,rr_bmi,rr_education Optional mortality inputs, as passed
#'   to [simeq_run_simulation()]. When all three are given, the earlier-wave
#'   percentiles are survival-weighted (see Details). When all are `NULL`
#'   (the default), the raw survey change is used.
#'
#' @return A `simeq_bmi_percentile_model`: a list with `models` (one `lm` per
#'   percentile), `probs`, `periods`, `projection_period` and `cohorts` (the
#'   cohort-by-percentile annual changes used in fitting).
#' @export
simeq_fit_bmi_transition_percentile <- function(
    bmi_panel,
    probs = seq(0.1, 0.9, by = 0.1),
    min_n = 30L,
    projection_period = NULL,
    max_age = 84L,
    sex_interactions = FALSE,
    life_table = NULL,
    rr_bmi = NULL,
    rr_education = NULL
) {
  required_columns <- c(
    "year", "sex", "age", "education_level2", "survey_weight", "bmi"
  )
  missing_columns <- setdiff(required_columns, names(bmi_panel))

  if (length(missing_columns) > 0L) {
    stop(
      "`bmi_panel` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (
    !is.numeric(probs) || length(probs) < 2L || anyNA(probs) ||
    any(probs <= 0 | probs >= 1) || is.unsorted(probs, strictly = TRUE)
  ) {
    stop(
      "`probs` must be at least two strictly increasing values in (0, 1).",
      call. = FALSE
    )
  }

  survey_years <- sort(unique(as.integer(bmi_panel$year)))

  if (length(survey_years) < 2L) {
    stop("`bmi_panel` must contain at least two survey years.", call. = FALSE)
  }

  bmi_panel <- bmi_panel[
    is.finite(bmi_panel$bmi) & is.finite(bmi_panel$survey_weight) &
      bmi_panel$survey_weight > 0,
    ,
    drop = FALSE
  ]

  mortality_inputs <- list(life_table, rr_bmi, rr_education)
  survival_weighted <- !all(vapply(mortality_inputs, is.null, logical(1)))

  if (survival_weighted) {
    if (any(vapply(mortality_inputs, is.null, logical(1)))) {
      stop(
        "Supply all of `life_table`, `rr_bmi` and `rr_education` to ",
        "survival-weight the transitions, or none of them.",
        call. = FALSE
      )
    }

    simeq_validate_mortality_risks(
      rr_bmi = rr_bmi,
      rr_education = rr_education
    )
  }

  cohort_rows <- list()

  for (i in seq_len(length(survey_years) - 1L)) {
    year_t <- survey_years[i]
    year_t1 <- survey_years[i + 1L]
    interval <- year_t1 - year_t
    wave_t <- bmi_panel[bmi_panel$year == year_t, , drop = FALSE]
    wave_t1 <- bmi_panel[bmi_panel$year == year_t1, , drop = FALSE]

    if (survival_weighted) {
      wave_t$survey_weight <- wave_t$survey_weight * .interval_survival(
        wave = wave_t,
        start_year = year_t,
        interval = interval,
        life_table = life_table,
        rr_bmi = rr_bmi,
        rr_education = rr_education,
        max_age = max_age
      )
    }

    for (age_start in seq(20L, as.integer(max_age) - 4L, by = 5L)) {
      if (age_start + interval + 4L > max_age) {
        next
      }

      for (sex in c("men", "women")) {
        for (education in sort(unique(bmi_panel$education_level2))) {
          baseline <- wave_t[
            wave_t$sex == sex & wave_t$education_level2 == education &
              wave_t$age >= age_start & wave_t$age <= age_start + 4L,
            ,
            drop = FALSE
          ]
          follow_up <- wave_t1[
            wave_t1$sex == sex & wave_t1$education_level2 == education &
              wave_t1$age >= age_start + interval &
              wave_t1$age <= age_start + interval + 4L,
            ,
            drop = FALSE
          ]

          if (nrow(baseline) < min_n || nrow(follow_up) < min_n) {
            next
          }

          q_t <- .weighted_quantile(baseline$bmi, baseline$survey_weight, probs)
          q_t1 <- .weighted_quantile(
            follow_up$bmi, follow_up$survey_weight, probs
          )

          cohort_rows[[length(cohort_rows) + 1L]] <- data.frame(
            period = year_t,
            year_t1 = year_t1,
            sex = sex,
            education_level2 = education,
            age_group_start = age_start,
            age_t = stats::weighted.mean(baseline$age, baseline$survey_weight),
            prob = probs,
            annual_bmi_change = (q_t1 - q_t) / interval,
            n_t = nrow(baseline),
            n_t1 = nrow(follow_up),
            transition_weight = sqrt(
              sum(baseline$survey_weight) * sum(follow_up$survey_weight)
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(cohort_rows) == 0L) {
    stop(
      "No synthetic cohort had at least `min_n` respondents at both waves.",
      call. = FALSE
    )
  }

  cohorts <- do.call(rbind, cohort_rows)
  periods <- sort(unique(cohorts$period))

  if (is.null(projection_period)) {
    projection_period <- max(periods)
  }

  if (!as.integer(projection_period) %in% periods) {
    stop(
      "`projection_period` must be one of the fitted periods: ",
      paste(periods, collapse = ", "),
      call. = FALSE
    )
  }

  # Same predictors as the mean model; a factor with a single observed level
  # (for example one survey period) is left out.
  model_terms <- c("age_t", "I(age_t^2)")
  for (factor_term in c("sex", "education_level2", "period")) {
    if (length(unique(cohorts[[factor_term]])) > 1L) {
      model_terms <- c(factor_term, model_terms)
    }
  }
  if (isTRUE(sex_interactions) && "sex" %in% model_terms) {
    model_terms <- c(
      model_terms,
      paste0("sex:", setdiff(model_terms, c("sex", "education_level2")))
    )
  }
  model_formula <- stats::reformulate(model_terms, "annual_bmi_change")

  models <- lapply(probs, function(p) {
    model_data <- cohorts[cohorts$prob == p, , drop = FALSE]
    model_data$sex <- factor(model_data$sex, levels = c("men", "women"))
    model_data$education_level2 <- factor(model_data$education_level2)
    model_data$period <- factor(model_data$period, levels = periods)
    stats::lm(
      model_formula,
      data = model_data,
      weights = transition_weight
    )
  })

  structure(
    list(
      models = models,
      probs = probs,
      periods = as.integer(periods),
      projection_period = as.integer(projection_period),
      cohorts = cohorts,
      survival_weighted = survival_weighted,
      sex_interactions = isTRUE(sex_interactions)
    ),
    class = "simeq_bmi_percentile_model"
  )
}

#' Update BMI with percentile-specific transitions
#'
#' Each person's rank within their sex, two-level education and five-year age
#' group is computed from the current survey-weighted BMI distribution. The
#' annual change is predicted at every modelled percentile for that group and
#' interpolated linearly at the person's rank. Ranks below the lowest or above
#' the highest modelled percentile take the change of that percentile.
#'
#' @param state A living population state with `sex`, `age`,
#'   `education_level2`, `survey_weight` and `bmi`.
#' @param bmi_percentile_model Output from
#'   [simeq_fit_bmi_transition_percentile()].
#' @param period Survey period whose period effect is used. `NULL` uses the
#'   model's `projection_period`.
#' @param bmi_limits Lower and upper permitted BMI values.
#'
#' @return `state` with updated `bmi`, `bmi_category`, `obesity` and, when
#'   present, `weight`.
#' @export
simeq_update_bmi_percentile <- function(
    state,
    bmi_percentile_model,
    period = NULL,
    bmi_limits = c(10, 80)
) {
  if (!inherits(bmi_percentile_model, "simeq_bmi_percentile_model")) {
    stop(
      "`bmi_percentile_model` must come from ",
      "`simeq_fit_bmi_transition_percentile()`.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "sex", "age", "education_level2", "survey_weight", "bmi"
  )
  missing_columns <- setdiff(required_columns, names(state))

  if (length(missing_columns) > 0L) {
    stop(
      "`state` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(state) == 0L) {
    return(state)
  }

  if (is.null(period)) {
    period <- bmi_percentile_model$projection_period
  }

  if (!as.integer(period) %in% bmi_percentile_model$periods) {
    stop(
      "`period` must be one of the fitted periods: ",
      paste(bmi_percentile_model$periods, collapse = ", "),
      call. = FALSE
    )
  }

  age_group_start <- pmin((as.integer(state$age) - 20L) %/% 5L * 5L + 20L, 80L)
  group_id <- paste(state$sex, state$education_level2, age_group_start)

  # Rank within group: mid-point of each person's share of the group's
  # cumulative survey weight, ordered by BMI (ties broken at random).
  rank <- numeric(nrow(state))
  for (rows in split(seq_len(nrow(state)), group_id)) {
    ordered <- rows[order(state$bmi[rows], stats::runif(length(rows)))]
    w <- state$survey_weight[ordered]
    rank[ordered] <- (cumsum(w) - w / 2) / sum(w)
  }

  # Predict each percentile's change once per sex x education x age.
  cell_key <- paste(state$sex, state$education_level2, state$age)
  cells <- unique(data.frame(
    key = cell_key,
    sex = state$sex,
    education_level2 = state$education_level2,
    age_t = as.numeric(state$age),
    stringsAsFactors = FALSE
  ))
  newdata <- cells
  newdata$sex <- factor(newdata$sex, levels = c("men", "women"))
  education_levels <- bmi_percentile_model$models[[1L]]$xlevels$education_level2
  if (is.null(education_levels)) {
    education_levels <- unique(bmi_percentile_model$cohorts$education_level2)
  }
  newdata$education_level2 <- factor(
    newdata$education_level2,
    levels = education_levels
  )
  newdata$period <- factor(
    as.integer(period),
    levels = bmi_percentile_model$periods
  )
  predicted <- vapply(
    bmi_percentile_model$models,
    function(model) as.numeric(stats::predict(model, newdata = newdata)),
    numeric(nrow(cells))
  )
  predicted <- matrix(predicted, nrow = nrow(cells))
  person_predicted <- predicted[match(cell_key, cells$key), , drop = FALSE]

  probs <- bmi_percentile_model$probs
  clamped <- pmin(pmax(rank, probs[1L]), probs[length(probs)])
  lower <- findInterval(clamped, probs, rightmost.closed = TRUE)
  lower <- pmin(lower, length(probs) - 1L)
  upper <- lower + 1L
  fraction <- (clamped - probs[lower]) / (probs[upper] - probs[lower])
  row_index <- seq_len(nrow(state))
  change <- person_predicted[cbind(row_index, lower)] * (1 - fraction) +
    person_predicted[cbind(row_index, upper)] * fraction

  state$bmi <- as.numeric(
    pmin(pmax(state$bmi + change, bmi_limits[1L]), bmi_limits[2L])
  )
  state$bmi_category <- simeq_bmi_category(state$bmi)
  state$obesity <- state$bmi >= 30

  .refresh_weight_from_bmi(state)
}

# Probability that each respondent of a survey wave survives `interval`
# years under the simulation's mortality model, with BMI fixed at its
# observed value. The life-table hazard of each year is rescaled so its
# survey-weighted mean within sex and five-year age group matches the life
# table, as in simeq_calculate_death_probability().
.interval_survival <- function(
    wave,
    start_year,
    interval,
    life_table,
    rr_bmi,
    rr_education,
    max_age
) {
  relative_risk <- rr_bmi$rr_bmi[
    match(
      paste(wave$sex, simeq_bmi_category(wave$bmi)),
      paste(rr_bmi$sex, rr_bmi$bmi_category)
    )
  ] * .education_relative_risk(wave, rr_education)

  if (anyNA(relative_risk)) {
    stop(
      "Missing mortality relative risks for some survey respondents.",
      call. = FALSE
    )
  }

  cumulative_hazard <- numeric(nrow(wave))

  for (k in seq_len(interval) - 1L) {
    age_k <- pmin(as.integer(wave$age) + k, as.integer(max_age))
    band_k <- paste(wave$sex, (age_k - 20L) %/% 5L)
    year_table <- simeq_select_life_table_year(life_table, start_year + k)
    qx <- year_table$qx[
      match(paste(age_k, wave$sex), paste(year_table$age, year_table$sex))
    ]

    if (anyNA(qx)) {
      stop("Missing life-table `qx` for some survey respondents.", call. = FALSE)
    }

    mean_rr <- tapply(wave$survey_weight * relative_risk, band_k, sum) /
      tapply(wave$survey_weight, band_k, sum)
    cumulative_hazard <- cumulative_hazard +
      -log(1 - qx) / as.numeric(mean_rr[band_k]) * relative_risk
  }

  exp(-cumulative_hazard)
}

.weighted_quantile <- function(x, w, probs) {
  o <- order(x)
  x <- x[o]
  w <- w[o]
  cumulative <- (cumsum(w) - w / 2) / sum(w)
  stats::approx(cumulative, x, xout = probs, rule = 2, ties = "ordered")$y
}
