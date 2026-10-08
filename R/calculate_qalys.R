#' Calculate annual QALYs from EQ-5D norms, education, and BMI category
#'
#' Calculates annual quality-adjusted life years (QALYs) for alive simulated
#' people aged 20 to 84 years.
#'
#' Individual utility is derived in three steps:
#' 1. **Back-calculate the healthy-weight utility** (U_h) for each Ferreira
#'    age band from the published EQ-5D population norm and the reference BMI
#'    prevalence. Because the population norm is a weighted average across all
#'    BMI categories, it already incorporates the effect of overweight and
#'    obesity. Applying decrements directly to the norm would double-penalise
#'    individuals with higher BMI. The back-calculation is:
#'    `U_h = U_norm + sum(|decrement_k| * prevalence_k)` for overweight and
#'    each obesity sub-category.
#' 2. **Apply the BMI decrement** to obtain the category-specific utility:
#'    `U_bmi = U_h + bmi_decrement` (decrement is 0 for normal/underweight,
#'    negative for overweight and obesity).
#' 3. **Apply the education adjustment** as a shift relative to the mean
#'    education-level utility across the population.
#'
#' People who die during a cycle are recorded in the following year with
#' `alive = FALSE`. By default (`half_cycle_deaths = TRUE`) they contribute
#' half a year at their utility in that year, instead of zero.
#'
#' Two-level education (`"lower"`, `"higher"`) is also accepted. `"lower"`
#' uses the mean of the Ferreira low and medium adjustments and `"higher"`
#' uses the high adjustment.
#'
#' Age-specific (not sex-specific) EQ-5D norms are used, based on Ferreira
#' et al. (2023) Portuguese EQ-5D-5L population norms. BMI decrements are
#' from the Carrello et al. (2021) meta-analysis (pooled, mainly SF-6D and AQoL):
#' overweight 0.020 (95% CI 0.010-0.030), obesity class I 0.047 (0.017-0.077),
#' class II 0.072 (0.028-0.116), class III 0.084 (0.039-0.130). No decrement is
#' applied to underweight in the base case. Pass `bmi_decrements` to run
#' sensitivity analyses (for example the CI bounds).
#'
#' @param simulation_output A data frame returned by [simeq_run_simulation()].
#' @param min_age Minimum age included in QALY calculations.
#' @param max_age Maximum age included in QALY calculations.
#' @param reference_bmi_prevalence A data frame with columns `age_band`
#'   (one of `"20_29"`, `"30_49"`, `"50_69"`, `"70_84"`), `bmi_category`
#'   (one of `"overweight"`, `"obesity_I"`, `"obesity_II"`, `"obesity_III"`),
#'   and `prevalence` (proportion, 0–1). Used to back-calculate the
#'   healthy-weight utility for each Ferreira norm age band. Defaults to
#'   approximate Portuguese NHS 2019 values; replace with exact values
#'   computed from your own survey data using `data-raw/prepare_model_inputs.R`.
#' @param bmi_decrements Optional named numeric vector of utility decrements
#'   (negative values) replacing the Carrello et al. (2021) defaults. Names must
#'   be among `underweight`, `normal_weight`, `overweight`, `obesity_I`,
#'   `obesity_II`, `obesity_III`; categories not supplied keep their default.
#'   The same values are used to back-calculate the healthy-weight utility.
#' @param half_cycle_deaths Logical. If `TRUE` (default), people who died
#'   during the cycle contribute half a person-year in their death year.
#' @param discount_rate Annual discount rate for the discounted QALY columns.
#'   Default 0 (no discounting).
#' @param discount_base_year Year from which QALYs are discounted. QALYs in
#'   or before this year have a discount factor of one. Defaults to the first
#'   year in `simulation_output`.
#'
#' @return A list containing:
#' \describe{
#'   \item{individual}{Individual annual utilities, person-years, weighted
#'     QALYs (undiscounted and discounted), and weighted QALYs lost to high BMI
#'     (the absolute BMI decrement times weight and person-years).}
#'   \item{by_year_sim}{Weighted QALY totals by simulation, year, sex, and
#'     education. `weighted_population` counts people alive at the end of the
#'     cycle; `weighted_person_years` adds half the weight of decedents when
#'     `half_cycle_deaths = TRUE`. `mean_annual_qaly_per_person` is total
#'     undiscounted QALYs divided by weighted person-years.}
#'   \item{by_year}{Mean weighted QALY totals and simulation quantiles by year, sex, and education.}
#'   \item{by_year_obesity_sim}{Weighted QALY totals and mean annual QALYs per person by obesity status.}
#'   \item{by_year_obesity}{Mean annual QALYs per person and simulation quantiles by obesity status.}
#' }
#'
#' @export
simeq_calculate_qalys <- function(
    simulation_output,
    min_age = 20L,
    max_age = 84L,
    reference_bmi_prevalence = data.frame(
      age_band     = rep(c("20_29", "30_49", "50_69", "70_84"), each = 4L),
      bmi_category = rep(
        c("overweight", "obesity_I", "obesity_II", "obesity_III"), 4L
      ),
      # Approximate Portuguese NHS 2019 values by age band.
      # Replace with exact values from prepare_model_inputs.R.
      prevalence = c(
        0.27, 0.07, 0.02, 0.01,   # 20-29
        0.34, 0.13, 0.04, 0.01,   # 30-49
        0.39, 0.17, 0.05, 0.01,   # 50-69
        0.38, 0.14, 0.04, 0.01    # 70-84
      ),
      stringsAsFactors = FALSE
    ),
    bmi_decrements = NULL,
    half_cycle_deaths = TRUE,
    discount_rate = 0,
    discount_base_year = NULL
) {
  required_columns <- c(
    "person_id",
    "simulation",
    "year",
    "age",
    "sex",
    "education_level",
    "survey_weight",
    "bmi_category",
    "alive"
  )

  missing_columns <- setdiff(required_columns, names(simulation_output))

  if (length(missing_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.numeric(min_age) || length(min_age) != 1L || is.na(min_age)) {
    stop("`min_age` must be one non-missing numeric value.", call. = FALSE)
  }

  if (!is.numeric(max_age) || length(max_age) != 1L || is.na(max_age)) {
    stop("`max_age` must be one non-missing numeric value.", call. = FALSE)
  }

  if (min_age > max_age) {
    stop("`min_age` cannot exceed `max_age`.", call. = FALSE)
  }

  if (!is.logical(half_cycle_deaths) || length(half_cycle_deaths) != 1L ||
      is.na(half_cycle_deaths)) {
    stop("`half_cycle_deaths` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(discount_rate) || length(discount_rate) != 1L ||
      !is.finite(discount_rate) || discount_rate < 0) {
    stop("`discount_rate` must be one non-negative number.", call. = FALSE)
  }

  if (is.null(discount_base_year)) {
    discount_base_year <- suppressWarnings(
      min(as.integer(simulation_output$year), na.rm = TRUE)
    )
  }

  if (!is.numeric(discount_base_year) || length(discount_base_year) != 1L) {
    stop("`discount_base_year` must be one year.", call. = FALSE)
  }

  empty_individual <- data.frame(
    person_id = character(),
    simulation = integer(),
    year = integer(),
    age = integer(),
    sex = character(),
    education_level = character(),
    survey_weight = numeric(),
    bmi_category = character(),
    alive = logical(),
    eq5d_norm = numeric(),
    education_utility_adjustment = numeric(),
    bmi_utility_decrement = numeric(),
    utility_with_bmi = numeric(),
    person_years = numeric(),
    discount_factor = numeric(),
    qaly_weighted = numeric(),
    qaly_weighted_discounted = numeric(),
    qaly_loss_weighted = numeric(),
    obesity_status = character(),
    stringsAsFactors = FALSE
  )

  empty_by_year_sim <- data.frame(
    simulation = integer(),
    year = integer(),
    sex = character(),
    education_level = character(),
    weighted_population = numeric(),
    weighted_person_years = numeric(),
    total_qaly_weighted = numeric(),
    total_qaly_weighted_discounted = numeric(),
    total_qaly_loss_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    stringsAsFactors = FALSE
  )

  empty_by_year <- data.frame(
    year = integer(),
    sex = character(),
    education_level = character(),
    mean_weighted_population = numeric(),
    mean_weighted_person_years = numeric(),
    mean_total_qalys_weighted = numeric(),
    mean_total_qalys_weighted_discounted = numeric(),
    mean_total_qaly_loss_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    qaly_total_lo = numeric(),
    qaly_total_hi = numeric(),
    qaly_per_person_lo = numeric(),
    qaly_per_person_hi = numeric(),
    stringsAsFactors = FALSE
  )

  empty_by_year_obesity_sim <- data.frame(
    simulation = integer(),
    year = integer(),
    sex = character(),
    education_level = character(),
    obesity_status = character(),
    weighted_population = numeric(),
    weighted_person_years = numeric(),
    total_qaly_weighted = numeric(),
    total_qaly_weighted_discounted = numeric(),
    total_qaly_loss_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    stringsAsFactors = FALSE
  )

  empty_by_year_obesity <- data.frame(
    year = integer(),
    sex = character(),
    education_level = character(),
    obesity_status = character(),
    mean_weighted_population = numeric(),
    mean_weighted_person_years = numeric(),
    mean_total_qalys_weighted = numeric(),
    mean_total_qalys_weighted_discounted = numeric(),
    mean_total_qaly_loss_weighted = numeric(),
    mean_annual_qaly_per_person = numeric(),
    qaly_total_lo = numeric(),
    qaly_total_hi = numeric(),
    qaly_per_person_lo = numeric(),
    qaly_per_person_hi = numeric(),
    stringsAsFactors = FALSE
  )

  # Living people count a full cycle. Decedents are kept only for the
  # half-cycle correction; they carry `alive = FALSE` in their death year.
  in_age_range <- !is.na(simulation_output$age) &
    simulation_output$age >= min_age &
    simulation_output$age <= max_age
  known_status <- !is.na(simulation_output$alive)
  keep_record <- in_age_range & known_status & (
    simulation_output$alive | half_cycle_deaths
  )

  living_population <- simulation_output[keep_record, , drop = FALSE]

  if (nrow(living_population) == 0L) {
    return(
      list(
        individual = empty_individual,
        by_year_sim = empty_by_year_sim,
        by_year = empty_by_year,
        by_year_obesity_sim = empty_by_year_obesity_sim,
        by_year_obesity = empty_by_year_obesity
      )
    )
  }

  if (anyNA(living_population$survey_weight) ||
      any(!is.finite(living_population$survey_weight)) ||
      any(living_population$survey_weight <= 0)) {
    stop(
      "Simulated people must have finite, positive `survey_weight` values.",
      call. = FALSE
    )
  }

  valid_education <- c("low", "medium", "high", "lower", "higher")
  invalid_education <- setdiff(
    unique(as.character(living_population$education_level)),
    valid_education
  )

  if (length(invalid_education) > 0L) {
    stop(
      "Unrecognised education level(s): ",
      paste(invalid_education, collapse = ", "),
      ". Expected: ",
      paste(valid_education, collapse = ", "),
      call. = FALSE
    )
  }

  # ── Ferreira EQ-5D-5L Portuguese population norms (age-specific) ──────────
  # Age bands: 20-29, 30-49, 50-69, 70-84 (Ferreira et al. 2023, Table 3)
  ferreira_norms <- c(
    "20_29" = 0.962,
    "30_49" = 0.919,
    "50_69" = 0.868,
    "70_84" = 0.790
  )

  ferreira_band <- dplyr::case_when(
    living_population$age <= 29L ~ "20_29",
    living_population$age <= 49L ~ "30_49",
    living_population$age <= 69L ~ "50_69",
    TRUE                          ~ "70_84"
  )

  age_norm <- ferreira_norms[ferreira_band]

  # ── BMI utility decrements (Carrello et al. 2021; cf. healthy weight) ──
  # Decrements are negative; underweight uses 0 in the base case.
  bmi_decrement_values <- c(
    underweight   =  0.000,
    normal_weight =  0.000,
    overweight    = -0.020,
    obesity_I     = -0.047,
    obesity_II    = -0.072,
    obesity_III   = -0.084
  )

  if (!is.null(bmi_decrements)) {
    if (!is.numeric(bmi_decrements) || is.null(names(bmi_decrements)) ||
        anyNA(bmi_decrements) ||
        !all(names(bmi_decrements) %in% names(bmi_decrement_values))) {
      stop(
        "`bmi_decrements` must be a named numeric vector with names among: ",
        paste(names(bmi_decrement_values), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    if (any(bmi_decrements > 0)) {
      stop("`bmi_decrements` must be zero or negative.", call. = FALSE)
    }
    bmi_decrement_values[names(bmi_decrements)] <- bmi_decrements
  }

  bmi_category <- simeq_harmonise_bmi_category(
    living_population$bmi_category
  )

  bmi_utility_decrement <- unname(bmi_decrement_values[bmi_category])

  if (anyNA(bmi_utility_decrement)) {
    invalid_bmi <- unique(bmi_category[is.na(bmi_utility_decrement)])
    stop(
      "Unrecognised BMI category/categories after harmonisation: ",
      paste(invalid_bmi, collapse = ", "),
      call. = FALSE
    )
  }

  # ── Back-calculate healthy-weight utility U_h ───────────────────────────
  # The Ferreira population norm is a weighted average across all BMI
  # categories. Applying BMI decrements directly to the norm would
  # double-penalise overweight and obese individuals. Instead, for each age
  # band b:
  #
  #   U_norm(b) = U_h(b) * P_hw
  #             + (U_h(b) - |δ_ow|) * P_ow
  #             + (U_h(b) - |δ_obI|) * P_obI + ...
  #   =>  U_h(b) = U_norm(b) + Σ |δ_k| * P_k
  #
  # reference_bmi_prevalence supplies P_k for the relevant BMI categories.
  valid_ref_cats <- c("overweight", "obesity_I", "obesity_II", "obesity_III")

  if (
    !is.data.frame(reference_bmi_prevalence) ||
    !all(c("age_band", "bmi_category", "prevalence") %in%
         names(reference_bmi_prevalence))
  ) {
    stop(
      "`reference_bmi_prevalence` must be a data frame with columns ",
      "`age_band`, `bmi_category`, and `prevalence`.",
      call. = FALSE
    )
  }

  ref_df <- reference_bmi_prevalence[
    reference_bmi_prevalence$bmi_category %in% valid_ref_cats,
    ,
    drop = FALSE
  ]
  ref_df$abs_decrement <- abs(
    bmi_decrement_values[as.character(ref_df$bmi_category)]
  )
  ref_df$contrib <- ref_df$abs_decrement * ref_df$prevalence

  correction_by_band <- tapply(ref_df$contrib, ref_df$age_band, sum)

  correction_per_person <- as.numeric(
    correction_by_band[ferreira_band]
  )

  if (anyNA(correction_per_person)) {
    missing_bands <- unique(ferreira_band[is.na(correction_per_person)])
    stop(
      "`reference_bmi_prevalence` is missing age band(s): ",
      paste(missing_bands, collapse = ", "),
      call. = FALSE
    )
  }

  # U_h: healthy-weight utility for each person's age band
  u_h <- age_norm + correction_per_person

  # ── Education utility adjustment ─────────────────────────────────────────
  # Deviation from the mean education-level utility (Ferreira et al.)
  # Two-level education: lower = mean of low and medium, higher = high.
  education_utility_adjustment <- unname(c(
    low    = 0.848 - 0.887,
    medium = 0.912 - 0.887,
    high   = 0.930 - 0.887,
    lower  = ((0.848 + 0.912) / 2) - 0.887,
    higher = 0.930 - 0.887
  )[as.character(living_population$education_level)])

  # ── Individual utility: U_h + education shift + BMI decrement ───────────
  utility_with_bmi <- pmin(
    pmax(u_h + education_utility_adjustment + bmi_utility_decrement, 0),
    1
  )

  individual <- living_population
  individual$bmi_category <- bmi_category
  individual$eq5d_norm <- age_norm
  individual$education_utility_adjustment <- education_utility_adjustment
  individual$bmi_utility_decrement <- bmi_utility_decrement
  individual$utility_with_bmi <- utility_with_bmi
  individual$person_years <- ifelse(individual$alive, 1, 0.5)
  individual$discount_factor <- ifelse(
    individual$year <= discount_base_year,
    1,
    1 / (1 + discount_rate)^(individual$year - discount_base_year)
  )
  individual$qaly_weighted <- utility_with_bmi *
    individual$survey_weight *
    individual$person_years
  individual$qaly_weighted_discounted <- individual$qaly_weighted *
    individual$discount_factor
  individual$qaly_loss_weighted <- abs(bmi_utility_decrement) *
    individual$survey_weight *
    individual$person_years
  individual$living_weight <- ifelse(
    individual$alive,
    individual$survey_weight,
    0
  )
  individual$person_year_weight <- individual$survey_weight *
    individual$person_years
  individual$obesity_status <- ifelse(
    individual$bmi_category %in% c("obesity_I", "obesity_II", "obesity_III"),
    "obesity",
    "no_obesity"
  )

  by_year_sim <- stats::aggregate(
    cbind(
      weighted_population = living_weight,
      weighted_person_years = person_year_weight,
      total_qaly_weighted = qaly_weighted,
      total_qaly_weighted_discounted = qaly_weighted_discounted,
      total_qaly_loss_weighted = qaly_loss_weighted
    ) ~ simulation + year + sex + education_level,
    data = individual,
    FUN = sum,
    na.rm = TRUE
  )

  by_year_sim$mean_annual_qaly_per_person <- with(
    by_year_sim,
    ifelse(
      weighted_person_years > 0,
      total_qaly_weighted / weighted_person_years,
      NA_real_
    )
  )

  by_year_obesity_sim <- stats::aggregate(
    cbind(
      weighted_population = living_weight,
      weighted_person_years = person_year_weight,
      total_qaly_weighted = qaly_weighted,
      total_qaly_weighted_discounted = qaly_weighted_discounted,
      total_qaly_loss_weighted = qaly_loss_weighted
    ) ~ simulation + year + sex + education_level + obesity_status,
    data = individual,
    FUN = sum,
    na.rm = TRUE
  )

  by_year_obesity_sim$mean_annual_qaly_per_person <- with(
    by_year_obesity_sim,
    ifelse(
      weighted_person_years > 0,
      total_qaly_weighted / weighted_person_years,
      NA_real_
    )
  )

  summarise_across_simulations <- function(data, grouping_variables) {
    group_key <- do.call(
      paste,
      c(data[grouping_variables], sep = "\r")
    )

    summary_rows <- lapply(unique(group_key), function(key) {
      group_data <- data[group_key == key, , drop = FALSE]

      data.frame(
        group_data[grouping_variables][1, , drop = FALSE],
        mean_weighted_population = mean(
          group_data$weighted_population,
          na.rm = TRUE
        ),
        mean_weighted_person_years = mean(
          group_data$weighted_person_years,
          na.rm = TRUE
        ),
        mean_total_qalys_weighted = mean(
          group_data$total_qaly_weighted,
          na.rm = TRUE
        ),
        mean_total_qalys_weighted_discounted = mean(
          group_data$total_qaly_weighted_discounted,
          na.rm = TRUE
        ),
        mean_total_qaly_loss_weighted = mean(
          group_data$total_qaly_loss_weighted,
          na.rm = TRUE
        ),
        mean_annual_qaly_per_person = mean(
          group_data$mean_annual_qaly_per_person,
          na.rm = TRUE
        ),
        qaly_total_lo = as.numeric(stats::quantile(
          group_data$total_qaly_weighted,
          probs = 0.025,
          na.rm = TRUE,
          names = FALSE
        )),
        qaly_total_hi = as.numeric(stats::quantile(
          group_data$total_qaly_weighted,
          probs = 0.975,
          na.rm = TRUE,
          names = FALSE
        )),
        qaly_per_person_lo = as.numeric(stats::quantile(
          group_data$mean_annual_qaly_per_person,
          probs = 0.025,
          na.rm = TRUE,
          names = FALSE
        )),
        qaly_per_person_hi = as.numeric(stats::quantile(
          group_data$mean_annual_qaly_per_person,
          probs = 0.975,
          na.rm = TRUE,
          names = FALSE
        )),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })

    do.call(rbind, summary_rows)
  }

  individual$living_weight <- NULL
  individual$person_year_weight <- NULL

  by_year <- summarise_across_simulations(
    data = by_year_sim,
    grouping_variables = c("year", "sex", "education_level")
  )

  by_year_obesity <- summarise_across_simulations(
    data = by_year_obesity_sim,
    grouping_variables = c(
      "year",
      "sex",
      "education_level",
      "obesity_status"
    )
  )

  rownames(by_year) <- NULL
  rownames(by_year_obesity) <- NULL

  list(
    individual = individual,
    by_year_sim = by_year_sim,
    by_year = by_year,
    by_year_obesity_sim = by_year_obesity_sim,
    by_year_obesity = by_year_obesity
  )
}
