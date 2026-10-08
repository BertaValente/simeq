#' Build BMI synthetic-cohort transitions
#'
#' Builds survey-weighted synthetic-cohort BMI transitions from consecutive
#' repeated cross-sectional survey waves. Each transition follows an age cohort:
#' the follow-up age group is shifted forward by the number of years between
#' survey waves. The function returns one weighted transition observation per
#' sex, education level, age cohort, and consecutive survey-wave pair.
#'
#' @param bmi_panel A prepared BMI panel containing `age_group`.
#'
#' @return A data frame of survey-weighted synthetic-cohort BMI transitions.
#' @export
#'
#' @examples
#' bmi_panel <- data.frame(
#'   year = c(2015L, 2015L, 2020L, 2020L),
#'   sex = c("men", "women", "men", "women"),
#'   age = c(20L, 20L, 25L, 25L),
#'   education_level2 = c("lower", "higher", "lower", "higher"),
#'   survey_weight = c(800, 900, 850, 950),
#'   bmi = c(22.1, 23.8, 25.4, 24.7)
#' )
#'
#' simeq_build_bmi_synthetic_cohorts(
#'   bmi_panel = simeq_prepare_bmi_panel(bmi_panel)
#' )
simeq_build_bmi_synthetic_cohorts <- function(
    bmi_panel
) {
  simeq_validate_bmi_panel(
    bmi_panel = bmi_panel
  )

  if (!"age_group" %in% names(bmi_panel)) {
    stop(
      "`bmi_panel` must contain an `age_group` column. ",
      "Use `simeq_prepare_bmi_panel()` first.",
      call. = FALSE
    )
  }

  expected_age_group <- simeq_bmi_age_group(
    age = bmi_panel$age
  )

  if (
    any(
      as.character(bmi_panel$age_group) !=
      expected_age_group
    )
  ) {
    stop(
      "`bmi_panel$age_group` must match the five-year group implied by `age`.",
      call. = FALSE
    )
  }

  survey_years <- sort(
    unique(
      as.integer(bmi_panel$year)
    )
  )

  if (length(survey_years) < 2L) {
    stop(
      "`bmi_panel` must contain at least two survey years.",
      call. = FALSE
    )
  }

  age_group_start <- function(x) {
    as.integer(
      sub(
        "_.*$",
        "",
        as.character(x)
      )
    )
  }

  summarise_one_wave <- function(data) {
    split_id <- interaction(
      data$sex,
      data$education_level2,
      data$age_group,
      drop = TRUE,
      lex.order = TRUE
    )

    groups <- split(
      data,
      split_id
    )

    summary_rows <- lapply(
      groups,
      function(group_data) {
        data.frame(
          sex = as.character(group_data$sex[1L]),
          education_level2 = as.character(
            group_data$education_level2[1L]
          ),
          age_group = as.character(group_data$age_group[1L]),
          age_group_start = age_group_start(
            group_data$age_group[1L]
          ),
          age_mean = stats::weighted.mean(
            group_data$age,
            group_data$survey_weight
          ),
          bmi_mean = stats::weighted.mean(
            group_data$bmi,
            group_data$survey_weight
          ),
          survey_weight_total = sum(
            group_data$survey_weight
          ),
          sample_n = nrow(group_data),
          stringsAsFactors = FALSE
        )
      }
    )

    do.call(
      rbind,
      summary_rows
    )
  }

  build_one_transition <- function(
    baseline_year,
    follow_up_year
  ) {
    interval_years <- as.integer(
      follow_up_year - baseline_year
    )

    baseline_summary <- summarise_one_wave(
      bmi_panel[
        bmi_panel$year == baseline_year,
        ,
        drop = FALSE
      ]
    )

    follow_up_summary <- summarise_one_wave(
      bmi_panel[
        bmi_panel$year == follow_up_year,
        ,
        drop = FALSE
      ]
    )

    # Map the expected follow-up age to the nearest 5-year group start.
    # Survey intervals (7, 9 years) are not multiples of 5, so the shifted
    # age (e.g. 20 + 7 = 27) must be snapped to the lower bound of its
    # 5-year group (27 → 25) to find a match in the follow-up wave.
    raw_follow_up_age <- baseline_summary$age_group_start + interval_years
    baseline_summary$expected_follow_up_start <- (
      (as.integer(raw_follow_up_age) - 20L) %/% 5L
    ) * 5L + 20L

    # Drop cohorts whose expected follow-up age group starts beyond 80
    # (the 80-84 group = start 80 is the last trackable cohort follow-up;
    # the 80-84 group itself cannot be a baseline because its follow-up
    # would be aged 87+ and is out of range).
    baseline_summary <- baseline_summary[
      baseline_summary$expected_follow_up_start <= 80L,
      ,
      drop = FALSE
    ]

    matched_data <- merge(
      baseline_summary,
      follow_up_summary,
      by.x = c(
        "sex",
        "education_level2",
        "expected_follow_up_start"
      ),
      by.y = c(
        "sex",
        "education_level2",
        "age_group_start"
      ),
      all = FALSE,
      suffixes = c("_t", "_t1")
    )

    if (nrow(matched_data) == 0L) {
      return(NULL)
    }

    data.frame(
      year_t = rep.int(
        as.integer(baseline_year),
        nrow(matched_data)
      ),
      year_t1 = rep.int(
        as.integer(follow_up_year),
        nrow(matched_data)
      ),
      interval_years = rep.int(
        interval_years,
        nrow(matched_data)
      ),
      sex = matched_data$sex,
      education_level2 = matched_data$education_level2,
      age_group_t = matched_data$age_group_t,
      age_group_t1 = matched_data$age_group_t1,
      age_t = matched_data$age_mean_t,
      age_t1 = matched_data$age_mean_t1,
      bmi_t = matched_data$bmi_mean_t,
      bmi_t1 = matched_data$bmi_mean_t1,
      annual_bmi_change = (
        matched_data$bmi_mean_t1 -
          matched_data$bmi_mean_t
      ) / interval_years,
      survey_weight_t = matched_data$survey_weight_total_t,
      survey_weight_t1 = matched_data$survey_weight_total_t1,
      transition_weight = sqrt(
        matched_data$survey_weight_total_t *
          matched_data$survey_weight_total_t1
      ),
      sample_n_t = matched_data$sample_n_t,
      sample_n_t1 = matched_data$sample_n_t1,
      stringsAsFactors = FALSE
    )
  }

  transition_data <- lapply(
    seq_len(length(survey_years) - 1L),
    function(i) {
      build_one_transition(
        baseline_year = survey_years[i],
        follow_up_year = survey_years[i + 1L]
      )
    }
  )

  transition_data <- Filter(
    Negate(is.null),
    transition_data
  )

  if (length(transition_data) == 0L) {
    stop(
      "No age-cohort matches were found across consecutive survey years.",
      call. = FALSE
    )
  }

  do.call(rbind, transition_data)
  # Note: synthetic cohorts are defined for baseline age groups 20-24 through
  # 75-79. The 75-79 group's follow-up lands in the 80-84 stratum, which is
  # observed in the data. Ages 80-84 as *baseline* cannot form a cohort
  # (follow-up would require data aged 87+); BMI change at those ages is
  # therefore extrapolated from the fitted quadratic age function.
}
