#' Calculate annual obesity-attributable costs from simulated BMI trajectories
#'
#' Projects the Borges et al. (2025) 2018 attributable direct medical costs
#' forward with the simulated BMI distribution.
#'
#' **Method.** For each year and disease group, the published attributable
#' cost is multiplied by `PAF(year) / PAF_high_BMI(base_cost_year)`, where both
#' PAFs are computed from the simulated population with the same method (see
#' [calculate_obesity_pafs_individual()]): each person's own BMI enters the GBD
#' 2019 relative risk per 5 kg/m² above `bmi_reference`. The base year
#' therefore reproduces the published costs exactly, and later years change
#' only with the simulated BMI distribution, not with differences between the
#' simulated and published PAF methods. With `exposure = "obesity"`,
#' `PAF(year)` counts only people with BMI of 30 or more, giving the share of
#' the published high-BMI costs attributable to obesity.
#'
#' Total disease costs are held at their 2018 level (apart from
#' `unit_cost_annual_growth`); population growth and ageing do not change
#' them. Direct treatment costs for overweight and obesity follow the
#' simulated prevalence of the exposure relative to the base year.
#'
#' **Unit-cost growth** (`unit_cost_annual_growth`) captures real growth in
#' healthcare unit costs beyond general price inflation (CPI). A value of 0
#' (default) assumes costs evolve with CPI only. A positive value (e.g. 0.01
#' for 1 %/year) applies compound real growth to the 2018 cost baseline before
#' CPI adjustment.
#'
#' @param simulation_output A data frame from [simeq_run_simulation()].
#' @param gbd_rr_matrix Relative-risk matrix as returned by
#'   [read_gbd_bmi_rr()].
#' @param costs_dro Published obesity-attributable direct medical costs by
#'   disease group, as returned by [obesity_direct_costs_2018()].
#' @param outcome_mapping GBD outcome-to-cost-group mapping from
#'   [obesity_cost_outcome_mapping()].
#' @param exclude_disease_groups Character vector of disease groups to exclude
#'   (default includes `"obesity_overweight"` and `"depression"`).
#' @param years Integer vector of years to compute. Defaults to all years with
#'   living simulated individuals.
#' @param exposure `"high_bmi"` (default, BMI of 25 or more, as in Borges et
#'   al.) or `"obesity"` (BMI of 30 or more).
#' @param bmi_reference Reference BMI for RR evaluation (kg/m²). Default 22.
#' @param weight_col Name of the survey-weight column. Default
#'   `"survey_weight"`.
#' @param base_cost_year Year of the published cost baseline. Default 2018L.
#' @param unit_cost_annual_growth Annual real growth in unit costs beyond
#'   general CPI, as a proportion (e.g. 0.01 = 1 %/year). Default 0.
#' @param cpi_2026 CPI index value for 2026 relative to 2018 = 100. Used to
#'   convert constant-2018 costs to 2026 nominal euros. Default 117.22
#'   (retained for backward compatibility; update to the current Eurostat
#'   value when available).
#' @param cpi_table Optional data frame with columns `year` (integer) and
#'   `cpi_index` (numeric, 2018 = 100). When supplied, each year's cost is
#'   also expressed in that year's nominal price level and returned as
#'   `total_keur_year_nominal`. The 2026 value in this table (if present)
#'   takes precedence over `cpi_2026` for the `total_keur_2026_nominal`
#'   column; otherwise `cpi_2026` is used as the fallback.
#' @param discount_rate Annual discount rate applied after `discount_base_year`.
#'   Default 0.03.
#' @param discount_base_year Year from which future costs are discounted.
#'   Default 2026L.
#' @param include_direct_costs Logical. If `TRUE` (default), the published
#'   direct treatment costs for overweight and obesity (`direct_cost_group`)
#'   are added at 100% attribution, scaled by the simulated exposure
#'   prevalence relative to `base_cost_year`. They
#'   must also be listed in `exclude_disease_groups` so they are not
#'   PAF-rescaled as well.
#' @param direct_cost_group Disease group holding the direct treatment costs.
#'   Default `"obesity_overweight"`.
#' @param by_simulation Logical. If `TRUE` and `simulation_output` has a
#'   `simulation` column, costs are also computed separately for each
#'   replication, and `totals` gains 2.5th and 97.5th percentile columns
#'   (95% Monte Carlo simulation intervals). Default `FALSE`.
#'
#' @return A list with:
#' \describe{
#'   \item{by_year_disease}{Tidy data frame of annual attributable costs by
#'     year and disease group, including `paf_obesity`, `paf_reference`, and
#'     `paf_ratio` columns.}
#'   \item{totals}{Annual costs summed across disease groups, with 2018-constant,
#'     2026-nominal, and 2026-present-value columns.}
#'   \item{by_sex_disease}{Annual attributable costs by year, sex, and disease
#'     group. The national cost of each group is allocated between sexes in
#'     proportion to each sex's PAF times its weighted population, so men and
#'     women add up to the national total. Direct treatment costs are
#'     allocated by each sex's weighted high-BMI population.}
#'   \item{by_sex_totals}{Annual costs summed across disease groups by sex,
#'     with 2018-constant, 2026-nominal, and 2026-present-value columns.}
#'   \item{totals_by_simulation}{Only when `by_simulation = TRUE`: annual
#'     2018-constant totals for each replication.}
#' }
#' @export
calculate_annual_obesity_costs <- function(
    simulation_output,
    gbd_rr_matrix,
    costs_dro,
    outcome_mapping,
    exclude_disease_groups  = c("obesity_overweight", "depression"),
    years                   = NULL,
    exposure                = c("high_bmi", "obesity"),
    bmi_reference           = 22,
    weight_col              = "survey_weight",
    base_cost_year          = 2018L,
    unit_cost_annual_growth = 0,
    cpi_2026                = 117.22,
    cpi_table               = NULL,
    discount_rate           = 0.03,
    discount_base_year      = 2026L,
    include_direct_costs    = TRUE,
    direct_cost_group       = "obesity_overweight",
    by_simulation           = FALSE
) {
  exposure <- match.arg(exposure)
  exposure_min_bmi <- if (identical(exposure, "obesity")) 30 else 25

  # ── Input validation ────────────────────────────────────────────────────────
  required_sim_cols <- c("year", "age", "sex", "bmi", "alive", weight_col)
  missing_sim <- setdiff(required_sim_cols, names(simulation_output))
  if (length(missing_sim) > 0L) {
    stop(
      "`simulation_output` is missing required column(s): ",
      paste(missing_sim, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(unit_cost_annual_growth) ||
      length(unit_cost_annual_growth) != 1L ||
      !is.finite(unit_cost_annual_growth) ||
      unit_cost_annual_growth < -1) {
    stop(
      "`unit_cost_annual_growth` must be a single finite number >= -1.",
      call. = FALSE
    )
  }

  if (!is.logical(include_direct_costs) || length(include_direct_costs) != 1L ||
      is.na(include_direct_costs)) {
    stop("`include_direct_costs` must be TRUE or FALSE.", call. = FALSE)
  }

  if (isTRUE(include_direct_costs)) {
    if (!direct_cost_group %in% as.character(costs_dro$disease_group)) {
      stop(
        "`direct_cost_group` '", direct_cost_group,
        "' is not a disease group in `costs_dro`.",
        call. = FALSE
      )
    }
    if (!direct_cost_group %in% exclude_disease_groups) {
      stop(
        "`direct_cost_group` must also be listed in `exclude_disease_groups` ",
        "so it is not counted twice.",
        call. = FALSE
      )
    }
  }

  if (!is.logical(by_simulation) || length(by_simulation) != 1L ||
      is.na(by_simulation)) {
    stop("`by_simulation` must be TRUE or FALSE.", call. = FALSE)
  }

  # ── Validate optional CPI table ─────────────────────────────────────────────
  if (!is.null(cpi_table)) {
    if (!is.data.frame(cpi_table) ||
        !all(c("year", "cpi_index") %in% names(cpi_table))) {
      stop(
        "`cpi_table` must be a data frame with columns `year` and `cpi_index`.",
        call. = FALSE
      )
    }
    cpi_table <- dplyr::transmute(
      cpi_table,
      year      = as.integer(year),
      cpi_index = as.numeric(cpi_index)
    )
    if (anyDuplicated(cpi_table$year) > 0L) {
      stop("`cpi_table` must have at most one row per year.", call. = FALSE)
    }
    if (any(!is.finite(cpi_table$cpi_index)) ||
        any(cpi_table$cpi_index <= 0)) {
      stop("`cpi_table$cpi_index` must contain finite positive values.",
           call. = FALSE)
    }
    # If 2026 is in the table, use it to override the scalar cpi_2026
    if (2026L %in% cpi_table$year) {
      cpi_2026 <- cpi_table$cpi_index[cpi_table$year == 2026L]
    }
  }

  # ── Determine years ─────────────────────────────────────────────────────────
  if (is.null(years)) {
    years <- sort(unique(
      simulation_output$year[simulation_output$alive]
    ))
  }
  years <- as.integer(years)

  sexes <- c("men", "women")
  cost_columns <- c("inpatient", "outpatient", "pharmacological", "total")
  cost_groups <- as.character(costs_dro$disease_group[
    !as.character(costs_dro$disease_group) %in% exclude_disease_groups
  ])

  .grow_costs <- function(yr) {
    unit_growth_factor <- (1 + unit_cost_annual_growth)^
      (yr - as.integer(base_cost_year))
    costs_yr <- costs_dro |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cost_columns),
          \(x) x * unit_growth_factor
        )
      )
    list(costs = costs_yr, unit_growth_factor = unit_growth_factor)
  }

  # Direct treatment costs scaled by the exposure-prevalence ratio `factor`.
  .direct_rows <- function(costs_yr, factor = 1) {
    costs_yr |>
      dplyr::filter(disease_group == direct_cost_group) |>
      dplyr::mutate(
        paf_obesity             = NA_real_,
        paf_reference           = NA_real_,
        paf_ratio               = factor,
        inpatient_obesity       = inpatient * factor,
        outpatient_obesity      = outpatient * factor,
        pharmacological_obesity = pharmacological * factor,
        total_obesity           = total * factor
      )
  }

  # PAF-rescaled attributable costs by disease group (same columns as
  # calculate_obesity_costs_age_sex()$costs_by_group).
  .group_cost_rows <- function(costs_yr, paf, reference) {
    rows <- costs_yr[
      !as.character(costs_yr$disease_group) %in% exclude_disease_groups,
      ,
      drop = FALSE
    ]
    group <- as.character(rows$disease_group)
    rows$paf_obesity <- unname(paf[group])
    rows$paf_reference <- unname(reference[group])
    rows |>
      dplyr::mutate(
        paf_ratio               = paf_obesity / paf_reference,
        inpatient_obesity       = inpatient       * paf_ratio,
        outpatient_obesity      = outpatient      * paf_ratio,
        pharmacological_obesity = pharmacological * paf_ratio,
        total_obesity           = total           * paf_ratio
      )
  }

  # ── Relative-risk sums, computed once ───────────────────────────────────────
  # The PAF of a stratum depends on the data only through two sums: the
  # weighted population and the weighted excess relative risk of each GBD
  # outcome. Both are computed once per replication x year with integer
  # keys; pooled, by-sex and per-replication PAFs then come from adding these
  # sums, so the large simulation output is read only once and never copied.
  per_simulation <- isTRUE(by_simulation) &&
    "simulation" %in% names(simulation_output)
  has_simulation <- "simulation" %in% names(simulation_output)
  simulation_ids <- if (has_simulation) {
    sort(unique(simulation_output$simulation))
  } else {
    1L
  }
  n_simulations <- length(simulation_ids)
  base_cost_year <- as.integer(base_cost_year)

  rr <- .cost_rr_strata(gbd_rr_matrix, outcome_mapping, cost_groups)

  .people_rows <- function(year_values) {
    weight <- simulation_output[[weight_col]]
    which(
      simulation_output$year %in% year_values &
        !is.na(simulation_output$alive) & simulation_output$alive &
        !is.na(simulation_output$age) &
        simulation_output$age >= 20L &
        simulation_output$age <= 84L &
        !is.na(simulation_output$bmi) &
        !is.na(weight) &
        weight > 0 &
        simulation_output$sex %in% sexes
    )
  }
  .simulation_index <- function(rows) {
    if (has_simulation) {
      match(simulation_output$simulation[rows], simulation_ids)
    } else {
      rep(1L, length(rows))
    }
  }

  # Base year, by replication: high-BMI reference PAFs (min BMI 25) and the
  # exposure prevalence used to scale direct costs.
  base_rows <- .people_rows(base_cost_year)
  base_simulation <- .simulation_index(base_rows)
  missing_base <- setdiff(seq_len(n_simulations), base_simulation)
  if (length(base_rows) == 0L || (per_simulation && length(missing_base) > 0L)) {
    stop(
      "`simulation_output` has no living people in `base_cost_year` (",
      base_cost_year, "), needed for the base-year reference.",
      call. = FALSE
    )
  }
  base_sums <- .cost_rr_sums(
    simulation_output, base_rows, base_simulation, n_simulations,
    rr, bmi_reference, min_bmi = 25, prevalence_min_bmi = exposure_min_bmi,
    weight_col = weight_col
  )

  # Costed years, by replication x year.
  year_rows <- .people_rows(years)
  year_key <- (.simulation_index(year_rows) - 1L) * length(years) +
    match(simulation_output$year[year_rows], years)
  year_sums <- .cost_rr_sums(
    simulation_output, year_rows, year_key, n_simulations * length(years),
    rr, bmi_reference, min_bmi = exposure_min_bmi,
    prevalence_min_bmi = exposure_min_bmi, weight_col = weight_col
  )
  key_simulation <- rep(seq_len(n_simulations), each = length(years))
  key_year <- rep(years, times = n_simulations)

  # ── Pooled costs (all replications) by year, with the sex allocation ───────
  pooled_reference <- .cost_group_pafs(
    .add_rr_sums(base_sums, rep(1L, n_simulations), 1L), rr
  )[1L, ]
  .check_group_pafs(pooled_reference, cost_groups)
  if (any(pooled_reference <= 0)) {
    stop(
      "The simulated base-year PAF is zero for: ",
      paste(cost_groups[pooled_reference <= 0], collapse = ", "),
      ". Exclude these groups.",
      call. = FALSE
    )
  }
  pooled_base_prevalence <- sum(base_sums$exposed) / sum(base_sums$population)

  pooled_year_sums <- .add_rr_sums(
    year_sums, match(key_year, years), length(years)
  )
  pooled_paf <- .cost_group_pafs(pooled_year_sums, rr)
  pooled_paf_sex <- lapply(
    stats::setNames(seq_along(sexes), sexes),
    function(sex_index) .cost_group_pafs(pooled_year_sums, rr, sex_index)
  )

  year_results <- lapply(seq_along(years), function(i) {
    yr <- years[[i]]
    grown <- .grow_costs(yr)

    if (sum(pooled_year_sums$population[i, ]) == 0) {
      stop("No living simulated people in year ", yr, ".", call. = FALSE)
    }
    paf_yr <- pooled_paf[i, ]
    .check_group_pafs(paf_yr, cost_groups)

    national <- .group_cost_rows(grown$costs, paf_yr, pooled_reference)
    if (isTRUE(include_direct_costs)) {
      direct_factor <- (sum(pooled_year_sums$exposed[i, ]) /
        sum(pooled_year_sums$population[i, ])) / pooled_base_prevalence
      national <- dplyr::bind_rows(
        national,
        .direct_rows(grown$costs, direct_factor)
      )
    }
    national <- national |>
      dplyr::mutate(year = yr, unit_growth_factor = grown$unit_growth_factor)

    # Sex allocation weights: PAF × weighted population for PAF-based
    # groups; weighted exposed population for direct costs.
    sex_weights <- lapply(seq_along(sexes), function(sex_index) {
      sex_strata <- rr$strata_sex == sex_index
      population_sex <- sum(pooled_year_sums$population[i, sex_strata])
      if (population_sex == 0) {
        stop("No living simulated people in year ", yr, ".", call. = FALSE)
      }

      weights <- data.frame(
        disease_group = cost_groups,
        sex = sexes[[sex_index]],
        allocation_weight = unname(pooled_paf_sex[[sex_index]][i, ]) *
          population_sex,
        stringsAsFactors = FALSE
      )

      if (isTRUE(include_direct_costs)) {
        weights <- rbind(
          weights,
          data.frame(
            disease_group = direct_cost_group,
            sex = sexes[[sex_index]],
            allocation_weight = sum(pooled_year_sums$exposed[i, sex_strata]),
            stringsAsFactors = FALSE
          )
        )
      }

      weights
    }) |>
      dplyr::bind_rows() |>
      dplyr::group_by(disease_group) |>
      dplyr::mutate(
        sex_share = if (sum(allocation_weight) > 0) {
          allocation_weight / sum(allocation_weight)
        } else {
          rep(1 / dplyr::n(), dplyr::n())
        }
      ) |>
      dplyr::ungroup() |>
      dplyr::select(disease_group, sex, sex_share)

    by_sex <- national |>
      dplyr::inner_join(sex_weights, by = "disease_group",
                        relationship = "one-to-many") |>
      dplyr::mutate(
        dplyr::across(
          c(inpatient_obesity, outpatient_obesity,
            pharmacological_obesity, total_obesity),
          \(x) x * sex_share
        )
      )

    list(national = national, by_sex = by_sex)
  })

  by_year_disease <- dplyr::bind_rows(lapply(year_results, `[[`, "national"))
  by_sex_disease <- dplyr::bind_rows(lapply(year_results, `[[`, "by_sex"))

  # ── Helper: attach year-specific CPI and discounting ────────────────────────
  .add_monetary_cols <- function(df, group_vars) {
    df <- df |>
      dplyr::mutate(
        total_keur_2026_nominal = total_keur_2018_constant * cpi_2026 / 100,
        discount_factor = dplyr::if_else(
          year <= as.integer(discount_base_year),
          1,
          1 / (1 + discount_rate)^(year - as.integer(discount_base_year))
        ),
        total_keur_2026_pv = total_keur_2026_nominal * discount_factor
      )
    if (!is.null(cpi_table)) {
      df <- df |>
        dplyr::left_join(cpi_table, by = "year") |>
        dplyr::mutate(
          total_keur_year_nominal = total_keur_2018_constant * cpi_index / 100
        ) |>
        dplyr::select(-cpi_index)
    }
    df
  }

  # ── Annual totals with CPI and discounting ───────────────────────────────────
  totals <- by_year_disease |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_keur_2018_constant = sum(total_obesity, na.rm = TRUE),
      .groups = "drop"
    ) |>
    .add_monetary_cols()

  by_sex_totals <- by_sex_disease |>
    dplyr::group_by(year, sex) |>
    dplyr::summarise(
      total_keur_2018_constant = sum(total_obesity, na.rm = TRUE),
      .groups = "drop"
    ) |>
    .add_monetary_cols()

  result <- list(
    by_year_disease = by_year_disease,
    totals          = totals,
    by_sex_disease  = by_sex_disease,
    by_sex_totals   = by_sex_totals
  )

  # ── Per-replication totals and 95% Monte Carlo simulation intervals ─────────
  # Same formulas as the pooled costs, from each replication's own sums.
  if (per_simulation) {
    reference <- .cost_group_pafs(base_sums, rr)
    .check_group_pafs(reference, cost_groups)
    if (any(reference <= 0)) {
      stop(
        "The simulated base-year PAF is zero for: ",
        paste(cost_groups[colSums(reference <= 0) > 0L], collapse = ", "),
        ". Exclude these groups.",
        call. = FALSE
      )
    }

    present <- rowSums(year_sums$population) > 0
    paf <- .cost_group_pafs(year_sums, rr)[present, , drop = FALSE]
    .check_group_pafs(paf, cost_groups)
    sim_index <- key_simulation[present]

    group_totals <- as.numeric(
      costs_dro$total[match(cost_groups, costs_dro$disease_group)]
    )
    growth <- (1 + unit_cost_annual_growth)^(key_year[present] - base_cost_year)
    total <- growth * as.vector(
      (paf / reference[sim_index, , drop = FALSE]) %*% group_totals
    )

    if (isTRUE(include_direct_costs)) {
      direct_total <- sum(as.numeric(
        costs_dro$total[costs_dro$disease_group == direct_cost_group]
      ))
      prevalence <- rowSums(year_sums$exposed[present, , drop = FALSE]) /
        rowSums(year_sums$population[present, , drop = FALSE])
      base_prevalence <- rowSums(base_sums$exposed) /
        rowSums(base_sums$population)
      total <- total + growth * direct_total * prevalence /
        base_prevalence[sim_index]
    }

    totals_by_simulation <- tibble::tibble(
      simulation = simulation_ids[sim_index],
      year = key_year[present],
      total_keur_2018_constant = total
    )

    intervals <- totals_by_simulation |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        total_keur_2018_constant_lo = stats::quantile(
          total_keur_2018_constant, 0.025, names = FALSE
        ),
        total_keur_2018_constant_hi = stats::quantile(
          total_keur_2018_constant, 0.975, names = FALSE
        ),
        .groups = "drop"
      )

    result$totals <- dplyr::left_join(result$totals, intervals, by = "year")
    result$totals_by_simulation <- totals_by_simulation
  }

  result
}


# ── Internal helpers for calculate_annual_obesity_costs() ────────────────────

.cost_sex_labels <- c("Males", "Females")
.cost_age_bands <- paste0(seq(20L, 80L, by = 5L), "-", seq(24L, 84L, by = 5L))

# GBD relative risks arranged by stratum (sex x five-year age band, 26
# strata numbered sex-major) and outcome, plus the outcome-to-cost-group map.
.cost_rr_strata <- function(gbd_rr_matrix, outcome_mapping, cost_groups) {
  rr_sex <- dplyr::recode(
    as.character(gbd_rr_matrix$sex),
    men   = "Males",  Male   = "Males",  male   = "Males",
    women = "Females", Female = "Females", female = "Females"
  )
  rr_stratum <- (match(rr_sex, .cost_sex_labels) - 1L) *
    length(.cost_age_bands) +
    match(as.character(gbd_rr_matrix$age_band), .cost_age_bands)
  outcomes <- unique(as.character(gbd_rr_matrix$disease))
  n_strata <- length(.cost_sex_labels) * length(.cost_age_bands)

  # log RR per 5 units; NA where an outcome has no RR for a stratum.
  log_rr <- matrix(NA_real_, n_strata, length(outcomes))
  matched <- !is.na(rr_stratum)
  log_rr[cbind(
    rr_stratum[matched],
    match(as.character(gbd_rr_matrix$disease[matched]), outcomes)
  )] <- log(as.numeric(gbd_rr_matrix$rr_per_5[matched]))

  mapping_outcome <- tolower(as.character(outcome_mapping$outcome_pattern))
  group_outcomes <- lapply(cost_groups, function(group) {
    columns <- match(
      mapping_outcome[outcome_mapping$disease_group == group],
      tolower(outcomes)
    )
    columns[!is.na(columns)]
  })

  list(
    log_rr = log_rr,
    outcomes = outcomes,
    strata_sex = rep(seq_along(.cost_sex_labels), each = length(.cost_age_bands)),
    group_outcomes = group_outcomes,
    cost_groups = cost_groups
  )
}

# Weighted population, exposed population and excess relative-risk sums by
# key and stratum for the people in `rows` of `simulation_output`.
.cost_rr_sums <- function(
    simulation_output, rows, key, n_keys, rr, bmi_reference, min_bmi,
    prevalence_min_bmi, weight_col, bmi_cap = 50, chunk_size = 250000L
) {
  n_strata <- nrow(rr$log_rr)
  n_outcomes <- ncol(rr$log_rr)
  population <- matrix(0, n_keys, n_strata)
  exposed <- population
  excess <- array(0, c(n_keys, n_strata, n_outcomes))

  for (start in seq.int(1L, max(length(rows), 1L), by = chunk_size)) {
    if (length(rows) == 0L) break
    chunk <- seq.int(start, min(start + chunk_size - 1L, length(rows)))
    r <- rows[chunk]
    k <- key[chunk]
    age <- as.integer(simulation_output$age[r])
    stratum <- (match(simulation_output$sex[r], c("men", "women")) - 1L) *
      length(.cost_age_bands) + (age - 20L) %/% 5L + 1L
    weight <- simulation_output[[weight_col]][r]
    bmi <- simulation_output$bmi[r]
    cell <- (stratum - 1L) * n_keys + k

    population <- population +
      matrix(.tabulate_weights(cell, weight, n_keys * n_strata), n_keys)
    exposed <- exposed + matrix(
      .tabulate_weights(cell, weight * (bmi >= prevalence_min_bmi),
                       n_keys * n_strata),
      n_keys
    )

    units <- ifelse(
      bmi >= min_bmi,
      pmax(pmin(bmi, bmi_cap) - bmi_reference, 0) / 5,
      0
    )
    is_exposed <- units > 0
    if (!any(is_exposed)) next
    log_rr <- rr$log_rr[stratum[is_exposed], , drop = FALSE]
    terms <- weight[is_exposed] * (exp(units[is_exposed] * log_rr) - 1)
    terms[is.na(terms)] <- 0
    sums <- rowsum(terms, cell[is_exposed], reorder = FALSE)
    cells <- as.integer(rownames(sums))
    for (o in seq_len(n_outcomes)) {
      excess[, , o][cells] <- excess[, , o][cells] + sums[, o]
    }
  }

  list(population = population, exposed = exposed, excess = excess)
}

.tabulate_weights <- function(cell, weight, n) {
  out <- numeric(n)
  sums <- rowsum(weight, cell, reorder = FALSE)
  out[as.integer(rownames(sums))] <- sums[, 1L]
  out
}

# Adds relative-risk sums over keys: `map[k]` is the new key of old key `k`.
.add_rr_sums <- function(sums, map, n_keys) {
  add <- function(x) {
    out <- rowsum(x, map, reorder = TRUE)
    full <- matrix(0, n_keys, ncol(x))
    full[as.integer(rownames(out)), ] <- out
    full
  }
  dims <- dim(sums$excess)
  excess <- add(matrix(sums$excess, dims[1L]))
  list(
    population = add(sums$population),
    exposed = add(sums$exposed),
    excess = array(excess, c(n_keys, dims[2L], dims[3L]))
  )
}

# Cost-group PAFs by key from relative-risk sums: stratum PAFs from the mean
# RR, population-weighted over strata for each outcome (strata with RRs and
# people only), then averaged over the outcomes mapped to each cost group.
# `sex_index` restricts the strata to one sex.
.cost_group_pafs <- function(sums, rr, sex_index = NULL) {
  n_keys <- nrow(sums$population)
  strata <- seq_len(nrow(rr$log_rr))
  if (!is.null(sex_index)) {
    strata <- strata[rr$strata_sex == sex_index]
  }

  paf_sum <- matrix(0, n_keys, length(rr$outcomes))
  weight_sum <- paf_sum
  for (st in strata) {
    population <- sums$population[, st]
    has_people <- population > 0
    if (!any(has_people)) next
    outcomes <- which(!is.na(rr$log_rr[st, ]))
    if (length(outcomes) == 0L) next
    mean_rr <- 1 + sums$excess[has_people, st, outcomes, drop = FALSE] /
      population[has_people]
    mean_rr <- matrix(mean_rr, sum(has_people))
    paf <- pmax((mean_rr - 1) / mean_rr, 0)
    paf_sum[has_people, outcomes] <- paf_sum[has_people, outcomes] +
      paf * population[has_people]
    weight_sum[has_people, outcomes] <- weight_sum[has_people, outcomes] +
      population[has_people]
  }
  paf_by_outcome <- paf_sum / weight_sum

  group_paf <- matrix(
    NA_real_, n_keys, length(rr$cost_groups),
    dimnames = list(NULL, rr$cost_groups)
  )
  for (g in seq_along(rr$cost_groups)) {
    columns <- rr$group_outcomes[[g]]
    if (length(columns) > 0L) {
      group_paf[, g] <- rowMeans(
        paf_by_outcome[, columns, drop = FALSE], na.rm = TRUE
      )
    }
  }
  group_paf[is.nan(group_paf)] <- NA_real_
  group_paf
}

# Stops when a cost group has no simulated PAF (no GBD outcome mapped to it
# had relative risks for the simulated strata).
.check_group_pafs <- function(paf, groups) {
  paf <- matrix(paf, ncol = length(groups))
  if (anyNA(paf)) {
    stop(
      "No simulated PAF available for non-excluded disease group(s): ",
      paste(groups[colSums(is.na(paf)) > 0L], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
