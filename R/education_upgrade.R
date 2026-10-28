#' Fit education upgrading after model entry
#'
#' People enter the model at age 20 with the education of NHS respondents
#' aged 20-24, but many complete higher education later. This function
#' estimates the annual probability that a person with medium (secondary)
#' education moves to high (tertiary) education, by sex and five-year age
#' group, from synthetic cohorts followed between consecutive NHS waves.
#'
#' For each pair of waves and each baseline age group, the gain in the
#' survey-weighted share with high education within the same birth cohort
#' (ages shifted by the interval) is annualised. Gains are pooled across the
#' selected periods, weighted by the geometric mean of the two waves' sample
#' sizes, and divided by the pooled baseline share with medium education.
#' Negative pooled gains give a rate of zero.
#'
#' @param bmi_panel Repeated cross-sectional data with `year`, `sex`, `age`,
#'   `education_level` (`low`, `medium`, `high`) and `survey_weight`.
#' @param periods Earlier-wave years of the wave pairs to use. `NULL` uses
#'   every consecutive pair.
#' @param age_groups Lower bounds of the five-year baseline age groups.
#'
#' @return A `simeq_education_upgrade` data frame with `sex`, `age_min`,
#'   `age_max` and `annual_rate`, and the cohort estimates in the `cohorts`
#'   attribute.
#' @export
simeq_fit_education_upgrade <- function(
    bmi_panel,
    periods = NULL,
    age_groups = c(20L, 25L, 30L)
) {
  required_columns <- c("year", "sex", "age", "education_level", "survey_weight")
  missing_columns <- setdiff(required_columns, names(bmi_panel))

  if (length(missing_columns) > 0L) {
    stop(
      "`bmi_panel` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  survey_years <- sort(unique(as.integer(bmi_panel$year)))

  if (length(survey_years) < 2L) {
    stop("`bmi_panel` must contain at least two survey years.", call. = FALSE)
  }

  pair_starts <- survey_years[-length(survey_years)]

  if (is.null(periods)) {
    periods <- pair_starts
  }

  if (!all(as.integer(periods) %in% pair_starts)) {
    stop(
      "`periods` must be earlier-wave years of consecutive waves: ",
      paste(pair_starts, collapse = ", "),
      call. = FALSE
    )
  }

  share <- function(data, level) {
    sum(data$survey_weight[data$education_level == level]) /
      sum(data$survey_weight)
  }

  cohort_rows <- list()

  for (year_t in as.integer(periods)) {
    year_t1 <- survey_years[match(year_t, survey_years) + 1L]
    interval <- year_t1 - year_t

    for (sex in c("men", "women")) {
      for (age_start in as.integer(age_groups)) {
        baseline <- bmi_panel[
          bmi_panel$year == year_t & bmi_panel$sex == sex &
            bmi_panel$age >= age_start & bmi_panel$age <= age_start + 4L,
          ,
          drop = FALSE
        ]
        follow_up <- bmi_panel[
          bmi_panel$year == year_t1 & bmi_panel$sex == sex &
            bmi_panel$age >= age_start + interval &
            bmi_panel$age <= age_start + interval + 4L,
          ,
          drop = FALSE
        ]

        if (nrow(baseline) == 0L || nrow(follow_up) == 0L) {
          next
        }

        cohort_rows[[length(cohort_rows) + 1L]] <- data.frame(
          period = year_t,
          interval = interval,
          sex = sex,
          age_min = age_start,
          high_t = share(baseline, "high"),
          high_t1 = share(follow_up, "high"),
          medium_t = share(baseline, "medium"),
          weight = sqrt(nrow(baseline) * nrow(follow_up)),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(cohort_rows) == 0L) {
    stop("No synthetic cohort could be formed.", call. = FALSE)
  }

  cohorts <- do.call(rbind, cohort_rows)
  cohorts$gain <- cohorts$high_t1 - cohorts$high_t

  groups <- unique(cohorts[, c("sex", "age_min")])
  rates <- lapply(seq_len(nrow(groups)), function(i) {
    rows <- cohorts[
      cohorts$sex == groups$sex[i] & cohorts$age_min == groups$age_min[i],
      ,
      drop = FALSE
    ]
    gain_per_year <- sum(rows$weight * rows$gain) /
      sum(rows$weight * rows$interval)
    medium_share <- stats::weighted.mean(rows$medium_t, rows$weight)

    data.frame(
      sex = groups$sex[i],
      age_min = groups$age_min[i],
      age_max = groups$age_min[i] + 4L,
      annual_rate = if (medium_share > 0) {
        min(max(gain_per_year / medium_share, 0), 1)
      } else {
        0
      },
      stringsAsFactors = FALSE
    )
  })

  rates <- do.call(rbind, rates)
  rates <- rates[order(rates$sex, rates$age_min), , drop = FALSE]
  rownames(rates) <- NULL
  attr(rates, "cohorts") <- cohorts
  class(rates) <- c("simeq_education_upgrade", class(rates))
  rates
}

#' Combine education upgrade rates by calendar period
#'
#' Stacks several [simeq_fit_education_upgrade()] results, each used for a
#' range of calendar years, so that upgrading can follow the period in which
#' it happens (for example rates from the 1998 to 2005 waves for 1998 to 2004
#' and rates pooled from later waves afterwards).
#'
#' @param periods A list of lists, each with `from` (first calendar year),
#'   `to` (last calendar year, `NA` for open-ended) and `upgrade` (output from
#'   [simeq_fit_education_upgrade()]). Ranges must not overlap.
#'
#' @return A `simeq_education_upgrade` data frame with `year_min` and
#'   `year_max` added. The year is the calendar year at the start of the
#'   annual cycle (the year of `state`).
#' @export
#'
#' @examples
#' early <- data.frame(sex = c("men", "women"), age_min = 20L, age_max = 24L,
#'                     annual_rate = c(0.01, 0.02))
#' late <- transform(early, annual_rate = c(0.02, 0.04))
#' simeq_combine_education_upgrade(list(
#'   list(from = 1998L, to = 2004L, upgrade = early),
#'   list(from = 2005L, to = NA, upgrade = late)
#' ))
simeq_combine_education_upgrade <- function(periods) {
  if (!is.list(periods) || length(periods) == 0L) {
    stop("`periods` must be a non-empty list.", call. = FALSE)
  }

  rows <- lapply(periods, function(p) {
    if (!all(c("from", "upgrade") %in% names(p))) {
      stop("Each period needs `from` and `upgrade`.", call. = FALSE)
    }
    to <- if (is.null(p$to) || is.na(p$to)) Inf else as.numeric(p$to)
    if (to < p$from) {
      stop("`to` must not be earlier than `from`.", call. = FALSE)
    }
    out <- as.data.frame(p$upgrade)[, c("sex", "age_min", "age_max", "annual_rate")]
    out$year_min <- as.numeric(p$from)
    out$year_max <- to
    out
  })

  rates <- do.call(rbind, rows)
  ranges <- unique(rates[, c("year_min", "year_max")])
  ranges <- ranges[order(ranges$year_min), , drop = FALSE]
  if (nrow(ranges) > 1L &&
      any(ranges$year_min[-1L] <= ranges$year_max[-nrow(ranges)])) {
    stop("Period year ranges must not overlap.", call. = FALSE)
  }

  rownames(rates) <- NULL
  class(rates) <- c("simeq_education_upgrade", "data.frame")
  rates
}

#' Move people from medium to high education
#'
#' Each living person with medium education whose sex and age match a row of
#' `education_upgrade` moves to high education with that row's annual
#' probability. Two-level education is updated to `"higher"` when present.
#' When `education_upgrade` has `year_min` and `year_max` columns (see
#' [simeq_combine_education_upgrade()]), only the rows whose range contains
#' the year of `state` are used.
#'
#' @param state A living population state with `sex`, `age` and
#'   `education_level` (and `year` when rates are period-specific).
#' @param education_upgrade Output from [simeq_fit_education_upgrade()] or
#'   [simeq_combine_education_upgrade()].
#'
#' @return `state` with updated `education_level` (and `education_level2`).
#' @export
simeq_update_education <- function(state, education_upgrade) {
  if (nrow(state) == 0L) {
    return(state)
  }

  if (all(c("year_min", "year_max") %in% names(education_upgrade))) {
    state_year <- unique(as.integer(state$year))
    if (length(state_year) != 1L) {
      stop(
        "`state` must contain one calendar year for period-specific rates.",
        call. = FALSE
      )
    }
    education_upgrade <- education_upgrade[
      education_upgrade$year_min <= state_year &
        education_upgrade$year_max >= state_year,
      ,
      drop = FALSE
    ]
  }

  rate <- numeric(nrow(state))

  for (i in seq_len(nrow(education_upgrade))) {
    in_group <- state$sex == education_upgrade$sex[i] &
      state$age >= education_upgrade$age_min[i] &
      state$age <= education_upgrade$age_max[i]
    rate[in_group] <- education_upgrade$annual_rate[i]
  }

  upgrade <- as.character(state$education_level) == "medium" &
    stats::runif(nrow(state)) < rate

  if (any(upgrade)) {
    if (is.factor(state$education_level)) {
      state$education_level <- as.character(state$education_level)
    }
    state$education_level[upgrade] <- "high"

    if ("education_level2" %in% names(state)) {
      if (is.factor(state$education_level2)) {
        state$education_level2 <- as.character(state$education_level2)
      }
      state$education_level2[upgrade] <- "higher"
    }
  }

  state
}
