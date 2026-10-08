#' Calculate obesity-attributable YLL, YLD and DALYs
#'
#' Calculates annual obesity prevalence, mortality population-attributable
#' fractions (PAFs), obesity-attributable years of life lost (YLL), years
#' lived with disability (YLD), and disability-adjusted life years (DALYs).
#'
#' @param simulation_output A data frame returned by
#'   [simeq_run_simulation()].
#' @param rr_bmi A BMI mortality relative-risk table containing `sex`,
#'   `bmi_category`, and `rr_bmi`.
#' @param disability_weight Disability weight applied to living people with
#'   BMI greater than or equal to 30. Either a single numeric scalar applied
#'   uniformly to all obesity classes, or a named numeric vector with elements
#'   `obesity_I`, `obesity_II`, and `obesity_III` for class-specific weights.
#' @param ref_age Reference life expectancy for YLL calculation. Either a
#'   single numeric scalar or a named vector `c(men = ..., women = ...)` for
#'   sex-specific values. Passed directly to [simeq_calculate_yll()].
#' @param min_age Minimum age included in burden calculations.
#' @param max_age Maximum age included in burden calculations.
#' @param ref_life_expectancy Optional remaining life expectancy table
#'   (`age`, `sex`, `ex`) passed to [simeq_calculate_yll()]. When supplied, it
#'   replaces `ref_age`.
#'
#' @return A list containing annual results by simulation and annual summaries
#'   averaged across simulations.
#'
#' @export
simeq_calculate_obesity_burden <- function(
    simulation_output,
    rr_bmi,
    disability_weight = 0.05,
    ref_age = 75L,
    min_age = 20L,
    max_age = 84L,
    ref_life_expectancy = NULL
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

  obesity_classes <- c("obesity_I", "obesity_II", "obesity_III")

  if (length(disability_weight) == 1L) {
    if (is.na(disability_weight) || disability_weight < 0 || disability_weight > 1) {
      stop(
        "`disability_weight` must be a number between 0 and 1.",
        call. = FALSE
      )
    }
    disability_weight <- stats::setNames(
      rep(disability_weight, 3L),
      obesity_classes
    )
  } else {
    if (!identical(sort(names(disability_weight)), sort(obesity_classes))) {
      stop(
        "`disability_weight` must be a named vector with elements ",
        paste(obesity_classes, collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (any(is.na(disability_weight)) || any(disability_weight < 0) || any(disability_weight > 1)) {
      stop(
        "All values in `disability_weight` must be between 0 and 1.",
        call. = FALSE
      )
    }
  }

  obesity_levels <- obesity_classes

  simulation_output$bmi_category <- simeq_harmonise_bmi_category(
    simulation_output$bmi_category
  )

  rr_bmi$bmi_category <- simeq_harmonise_bmi_category(
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

  yll_results <- simeq_calculate_yll(
    simulation_output = simulation_output,
    ref_age = ref_age,
    min_age = min_age,
    max_age = max_age,
    ref_life_expectancy = ref_life_expectancy
  )

  yll_by_year_sim <- yll_results$by_year_sim

  # Pre-split by group — O(n log n) vs the previous O(n²) which() loop
  group_indices <- split(
    seq_len(nrow(living_population)),
    interaction(
      living_population$simulation,
      living_population$year,
      living_population$sex,
      living_population$education_level,
      drop = TRUE
    )
  )

  # Pre-build a fast YLL lookup key rather than scanning yll_by_year_sim
  # for every group (avoids a second O(n²) search)
  if (nrow(yll_by_year_sim) > 0L) {
    yll_key <- paste(
      yll_by_year_sim$simulation,
      yll_by_year_sim$death_year,
      yll_by_year_sim$sex,
      yll_by_year_sim$education_level,
      sep = "\r"
    )
    yll_lookup <- tapply(
      yll_by_year_sim$total_yll_weighted,
      yll_key,
      sum,
      na.rm = TRUE
    )
  } else {
    yll_lookup <- NULL
  }

  # Deaths recorded in year t arise from the t-1 to t transition, with
  # mortality based on BMI at the start of that transition, so their
  # attributable share uses the PAF of the year t-1 population.
  paf_lookup <- vapply(
    group_indices,
    function(idx) {
      .obesity_mortality_paf(
        living_population[idx, , drop = FALSE],
        rr_bmi,
        obesity_levels
      )
    },
    numeric(1)
  )
  names(paf_lookup) <- vapply(
    group_indices,
    function(idx) {
      paste(
        living_population$simulation[idx[1]],
        living_population$year[idx[1]],
        living_population$sex[idx[1]],
        living_population$education_level[idx[1]],
        sep = "\r"
      )
    },
    character(1)
  )

  burden_rows <- lapply(
    group_indices,
    function(idx) {
      group_data <- living_population[idx, , drop = FALSE]

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

      obesity_category <- group_data$bmi_category %in%
        obesity_levels
      paf_obesity <- .obesity_mortality_paf(
        group_data, rr_bmi, obesity_levels
      )
      current_simulation <- group_data$simulation[1]
      current_year <- group_data$year[1]
      current_sex <- group_data$sex[1]
      current_education <- group_data$education_level[1]

      yll_total <- 0

      if (!is.null(yll_lookup)) {
        lookup_key <- paste(
          current_simulation, current_year,
          current_sex, current_education,
          sep = "\r"
        )
        hit <- yll_lookup[lookup_key]
        if (!is.na(hit)) yll_total <- hit
      }

      N_obesity_weighted <- sum(
        group_data$survey_weight[obesity_status],
        na.rm = TRUE
      )

      obese_rows <- group_data[obesity_category, ]
      yld_obesity_attrib <- sum(
        obese_rows$survey_weight *
          disability_weight[obese_rows$bmi_category],
        na.rm = TRUE
      )

      paf_for_deaths <- paf_lookup[paste(
        current_simulation, current_year - 1L,
        current_sex, current_education,
        sep = "\r"
      )]
      if (is.na(paf_for_deaths)) {
        paf_for_deaths <- paf_obesity
      }
      yll_obesity_attrib_weighted <- yll_total *
        unname(paf_for_deaths)

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

# Obesity all-cause mortality PAF for one group (exposed: obesity I-III).
.obesity_mortality_paf <- function(group_data, rr_bmi, obesity_levels) {
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

  paf_obesity
}
