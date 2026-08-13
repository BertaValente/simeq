#' Calculate obesity-attributable YLL, YLD and DALYs
#'
#' Calculates annual obesity prevalence, mortality population-attributable
#' fractions (PAFs), obesity-attributable years of life lost (YLL), years
#' lived with disability (YLD), and disability-adjusted life years (DALYs).
#'
#' @param simulation_output A data frame returned by
#'   [ponte_run_simulation()].
#' @param rr_bmi A BMI mortality relative-risk table containing `sex`,
#'   `bmi_category`, and `rr_bmi`.
#' @param disability_weight Disability weight applied to living people with
#'   BMI greater than or equal to 30.
#' @param ref_age Reference age for YLL calculation.
#' @param min_age Minimum age included in burden calculations.
#' @param max_age Maximum age included in burden calculations.
#'
#' @return A list containing annual results by simulation and annual summaries
#'   averaged across simulations.
#'
#' @export
ponte_calculate_obesity_burden <- function(
    simulation_output,
    rr_bmi,
    disability_weight = 0.05,
    ref_age = 75L,
    min_age = 20L,
    max_age = 84L
) {
  required_output_columns <- c(
    "person_id",
    "simulation",
    "year",
    "age",
    "sex",
    "education_level",
    "survey_weight",
    "bmi",
    "bmi_category",
    "alive",
    "death_year",
    "age_at_death"
  )

  missing_output_columns <- setdiff(
    required_output_columns,
    names(simulation_output)
  )

  if (length(missing_output_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(missing_output_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_rr_columns <- c(
    "sex",
    "bmi_category",
    "rr_bmi"
  )

  missing_rr_columns <- setdiff(
    required_rr_columns,
    names(rr_bmi)
  )

  if (length(missing_rr_columns) > 0L) {
    stop(
      "`rr_bmi` is missing required columns: ",
      paste(missing_rr_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (
    length(disability_weight) != 1L ||
    is.na(disability_weight) ||
    disability_weight < 0 ||
    disability_weight > 1
  ) {
    stop(
      "`disability_weight` must be one number between 0 and 1.",
      call. = FALSE
    )
  }

  obesity_levels <- c(
    "obesity_I",
    "obesity_II",
    "obesity_III"
  )

  simulation_output$bmi_category <- ponte_harmonise_bmi_category(
    simulation_output$bmi_category
  )

  rr_bmi$bmi_category <- ponte_harmonise_bmi_category(
    rr_bmi$bmi_category
  )

  living_population <- simulation_output[
    !is.na(simulation_output$alive) &
      simulation_output$alive &
      !is.na(simulation_output$age) &
      simulation_output$age >= min_age &
      simulation_output$age <= max_age,
    ,
    drop = FALSE
  ]

  if (nrow(living_population) == 0L) {
    empty_by_year_sim <- data.frame(
      simulation = integer(),
      year = integer(),
      sex = character(),
      education_level = character(),
      N_weighted = numeric(),
      prev_obesity = numeric(),
      paf_obesity = numeric(),
      yll_obesity_attrib_weighted = numeric(),
      yld_obesity_attrib = numeric(),
      daly_obesity_weighted = numeric(),
      yll_obesity_per100k = numeric(),
      yld_obesity_per100k = numeric(),
      daly_obesity_per100k = numeric(),
      stringsAsFactors = FALSE
    )

    empty_by_year <- data.frame(
      year = integer(),
      sex = character(),
      education_level = character(),
      N_weighted = numeric(),
      prev_obesity = numeric(),
      prev_obesity_lo = numeric(),
      prev_obesity_hi = numeric(),
      paf_obesity = numeric(),
      paf_obesity_lo = numeric(),
      paf_obesity_hi = numeric(),
      yll_obesity_attrib_weighted = numeric(),
      yld_obesity_attrib = numeric(),
      daly_obesity_weighted = numeric(),
      yll_obesity_per100k = numeric(),
      yll_obesity_per100k_lo = numeric(),
      yll_obesity_per100k_hi = numeric(),
      yld_obesity_per100k = numeric(),
      yld_obesity_per100k_lo = numeric(),
      yld_obesity_per100k_hi = numeric(),
      daly_obesity_per100k = numeric(),
      daly_obesity_per100k_lo = numeric(),
      daly_obesity_per100k_hi = numeric(),
      stringsAsFactors = FALSE
    )

    return(
      list(
        by_year_sim = empty_by_year_sim,
        by_year = empty_by_year
      )
    )
  }

  yll_results <- ponte_calculate_yll(
    simulation_output = simulation_output,
    ref_age = ref_age,
    min_age = min_age,
    max_age = max_age
  )

  yll_by_year_sim <- yll_results$by_year_sim

  living_keys <- paste(
    living_population$simulation,
    living_population$year,
    living_population$sex,
    living_population$education_level,
    sep = "\r"
  )

  group_keys <- unique(living_keys)

  burden_rows <- lapply(
    group_keys,
    function(key) {
      index <- which(living_keys == key)

      group_data <- living_population[
        index,
        ,
        drop = FALSE
      ]

      N_weighted <- sum(
        group_data$survey_weight,
        na.rm = TRUE
      )

      obesity_status <- !is.na(group_data$bmi) &
        group_data$bmi >= 30

      prev_obesity <- if (N_weighted > 0) {
        sum(
          group_data$survey_weight[obesity_status],
          na.rm = TRUE
        ) / N_weighted
      } else {
        NA_real_
      }

      rr_keys <- paste(
        rr_bmi$sex,
        rr_bmi$bmi_category,
        sep = "\r"
      )

      person_rr_keys <- paste(
        group_data$sex,
        group_data$bmi_category,
        sep = "\r"
      )

      matched_rr <- rr_bmi$rr_bmi[
        match(person_rr_keys, rr_keys)
      ]

      rr_for_paf <- rep(
        1,
        nrow(group_data)
      )

      obesity_category <- group_data$bmi_category %in%
        obesity_levels

      rr_for_paf[
        obesity_category &
          !is.na(matched_rr)
      ] <- matched_rr[
        obesity_category &
          !is.na(matched_rr)
      ]

      paf_denominator <- sum(
        group_data$survey_weight * rr_for_paf,
        na.rm = TRUE
      )

      paf_numerator <- sum(
        group_data$survey_weight *
          (rr_for_paf - 1),
        na.rm = TRUE
      )

      paf_obesity <- if (paf_denominator > 0) {
        max(
          min(
            paf_numerator / paf_denominator,
            1
          ),
          0
        )
      } else {
        0
      }

      current_simulation <- group_data$simulation[1]
      current_year <- group_data$year[1]
      current_sex <- group_data$sex[1]
      current_education <- group_data$education_level[1]

      yll_total <- 0

      if (nrow(yll_by_year_sim) > 0L) {
        yll_index <- which(
          yll_by_year_sim$simulation ==
            current_simulation &
            yll_by_year_sim$death_year ==
            current_year &
            yll_by_year_sim$sex ==
            current_sex &
            yll_by_year_sim$education_level ==
            current_education
        )

        if (length(yll_index) > 0L) {
          yll_total <- sum(
            yll_by_year_sim$total_yll_weighted[
              yll_index
            ],
            na.rm = TRUE
          )
        }
      }

      N_obesity_weighted <- sum(
        group_data$survey_weight[obesity_status],
        na.rm = TRUE
      )

      yld_obesity_attrib <- N_obesity_weighted *
        disability_weight

      yll_obesity_attrib_weighted <- yll_total *
        paf_obesity

      daly_obesity_weighted <- yll_obesity_attrib_weighted +
        yld_obesity_attrib

      yll_obesity_per100k <- if (N_weighted > 0) {
        yll_obesity_attrib_weighted /
          N_weighted * 1e5
      } else {
        NA_real_
      }

      yld_obesity_per100k <- if (N_weighted > 0) {
        yld_obesity_attrib /
          N_weighted * 1e5
      } else {
        NA_real_
      }

      daly_obesity_per100k <- if (N_weighted > 0) {
        daly_obesity_weighted /
          N_weighted * 1e5
      } else {
        NA_real_
      }

      data.frame(
        simulation = current_simulation,
        year = current_year,
        sex = current_sex,
        education_level = current_education,
        N_weighted = N_weighted,
        prev_obesity = prev_obesity,
        paf_obesity = paf_obesity,
        yll_obesity_attrib_weighted =
          yll_obesity_attrib_weighted,
        yld_obesity_attrib = yld_obesity_attrib,
        daly_obesity_weighted =
          daly_obesity_weighted,
        yll_obesity_per100k =
          yll_obesity_per100k,
        yld_obesity_per100k =
          yld_obesity_per100k,
        daly_obesity_per100k =
          daly_obesity_per100k,
        stringsAsFactors = FALSE
      )
    }
  )

  by_year_sim <- do.call(
    rbind,
    burden_rows
  )

  summary_keys <- paste(
    by_year_sim$year,
    by_year_sim$sex,
    by_year_sim$education_level,
    sep = "\r"
  )

  summary_rows <- lapply(
    unique(summary_keys),
    function(key) {
      index <- which(summary_keys == key)

      group_data <- by_year_sim[
        index,
        ,
        drop = FALSE
      ]

      data.frame(
        year = group_data$year[1],
        sex = group_data$sex[1],
        education_level =
          group_data$education_level[1],
        N_weighted = mean(
          group_data$N_weighted,
          na.rm = TRUE
        ),
        prev_obesity = mean(
          group_data$prev_obesity,
          na.rm = TRUE
        ),
        prev_obesity_lo = as.numeric(
          stats::quantile(
            group_data$prev_obesity,
            0.025,
            na.rm = TRUE
          )
        ),
        prev_obesity_hi = as.numeric(
          stats::quantile(
            group_data$prev_obesity,
            0.975,
            na.rm = TRUE
          )
        ),
        paf_obesity = mean(
          group_data$paf_obesity,
          na.rm = TRUE
        ),
        paf_obesity_lo = as.numeric(
          stats::quantile(
            group_data$paf_obesity,
            0.025,
            na.rm = TRUE
          )
        ),
        paf_obesity_hi = as.numeric(
          stats::quantile(
            group_data$paf_obesity,
            0.975,
            na.rm = TRUE
          )
        ),
        yll_obesity_attrib_weighted = mean(
          group_data$yll_obesity_attrib_weighted,
          na.rm = TRUE
        ),
        yld_obesity_attrib = mean(
          group_data$yld_obesity_attrib,
          na.rm = TRUE
        ),
        daly_obesity_weighted = mean(
          group_data$daly_obesity_weighted,
          na.rm = TRUE
        ),
        yll_obesity_per100k = mean(
          group_data$yll_obesity_per100k,
          na.rm = TRUE
        ),
        yll_obesity_per100k_lo = as.numeric(
          stats::quantile(
            group_data$yll_obesity_per100k,
            0.025,
            na.rm = TRUE
          )
        ),
        yll_obesity_per100k_hi = as.numeric(
          stats::quantile(
            group_data$yll_obesity_per100k,
            0.975,
            na.rm = TRUE
          )
        ),
        yld_obesity_per100k = mean(
          group_data$yld_obesity_per100k,
          na.rm = TRUE
        ),
        yld_obesity_per100k_lo = as.numeric(
          stats::quantile(
            group_data$yld_obesity_per100k,
            0.025,
            na.rm = TRUE
          )
        ),
        yld_obesity_per100k_hi = as.numeric(
          stats::quantile(
            group_data$yld_obesity_per100k,
            0.975,
            na.rm = TRUE
          )
        ),
        daly_obesity_per100k = mean(
          group_data$daly_obesity_per100k,
          na.rm = TRUE
        ),
        daly_obesity_per100k_lo = as.numeric(
          stats::quantile(
            group_data$daly_obesity_per100k,
            0.025,
            na.rm = TRUE
          )
        ),
        daly_obesity_per100k_hi = as.numeric(
          stats::quantile(
            group_data$daly_obesity_per100k,
            0.975,
            na.rm = TRUE
          )
        ),
        stringsAsFactors = FALSE
      )
    }
  )

  by_year <- do.call(
    rbind,
    summary_rows
  )

  list(
    by_year_sim = by_year_sim,
    by_year = by_year
  )
}
