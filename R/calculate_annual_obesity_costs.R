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
#'   Default 0.04.
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
    discount_rate           = 0.04,
    discount_base_year      = 2026L,
    include_direct_costs    = TRUE,
    direct_cost_group       = "obesity_overweight",
    by_simulation           = FALSE
) {
  exposure <- match.arg(exposure)
  exposure_min_bmi <- if (identical(exposure, "obesity")) 30 else 25

  # Disease PAFs from individual BMI for one data set and year.
  .pafs_for_year <- function(sim_data, yr, min_bmi = exposure_min_bmi) {
    calculate_obesity_pafs_individual(
      simulation_output = sim_data,
      year              = yr,
      rr_matrix         = gbd_rr_matrix,
      bmi_reference     = bmi_reference,
      exposure_min_bmi  = min_bmi,
      weight_col        = weight_col
    )
  }

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

  # Keep only the living person-years and columns the cost calculation uses.
  # Every helper below filters by year, so working on this reduced copy (and
  # one year at a time) gives the same results much faster.
  keep_columns <- intersect(
    c("simulation", required_sim_cols),
    names(simulation_output)
  )
  simulation_output <- simulation_output[
    !is.na(simulation_output$alive) & simulation_output$alive &
      simulation_output$year %in% c(years, as.integer(base_cost_year)),
    keep_columns,
    drop = FALSE
  ]

  sexes <- c("men", "women")
  cost_columns <- c("inpatient", "outpatient", "pharmacological", "total")

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

  # Direct treatment costs, included at 100% attribution.
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

  # Weighted national prevalence of the costed exposure.
  .exposure_prevalence <- function(prev) {
    exposed <- prev$p_obesity
    if (identical(exposure, "high_bmi")) {
      exposed <- exposed + prev$p_overweight
    }
    sum(prev$population_n * exposed) / sum(prev$population_n)
  }

  # High-BMI PAF by cost group in the base year of one data set.
  .base_year_reference <- function(sim_data) {
    prev_base <- extract_simulated_obesity_prevalence_by_age_sex(
      simulation_output = sim_data,
      year              = as.integer(base_cost_year),
      weight_col        = weight_col
    )
    pafs_base <- .pafs_for_year(
      sim_data, as.integer(base_cost_year), min_bmi = 25
    )
    groups <- calculate_obesity_costs_age_sex(
      paf_by_outcome         = pafs_base$paf_by_outcome,
      outcome_mapping        = outcome_mapping,
      costs_dro              = costs_dro,
      reference_paf_by_group = NULL,
      exclude_disease_groups = exclude_disease_groups
    )$costs_by_group
    if (any(groups$paf_obesity <= 0)) {
      stop(
        "The simulated base-year PAF is zero for: ",
        paste(groups$disease_group[groups$paf_obesity <= 0], collapse = ", "),
        ". Exclude these groups.",
        call. = FALSE
      )
    }
    list(
      reference = data.frame(
        disease_group = groups$disease_group,
        paf_reference = groups$paf_obesity,
        stringsAsFactors = FALSE
      ),
      exposure_prevalence = .exposure_prevalence(prev_base)
    )
  }

  # ── Costs for one data set (all replications pooled, or one replication) ───
  # `with_sex = FALSE` skips the sex allocation, which the per-replication
  # totals do not use.
  .costs_for_data <- function(sim_data, with_sex = TRUE) {
    if (!as.integer(base_cost_year) %in% sim_data$year[sim_data$alive]) {
      stop(
        "`simulation_output` has no living people in `base_cost_year` (",
        base_cost_year, "), needed for the base-year reference.",
        call. = FALSE
      )
    }
    base <- .base_year_reference(sim_data)
    reference_used <- base$reference

    data_by_year <- split(sim_data, sim_data$year)

    year_results <- lapply(years, function(yr) {
      grown <- .grow_costs(yr)
      year_data <- data_by_year[[as.character(yr)]]
      if (is.null(year_data)) {
        year_data <- sim_data[0L, , drop = FALSE]
      }

      # Age-sex BMI prevalence for year yr (overall)
      prev_yr <- extract_simulated_obesity_prevalence_by_age_sex(
        simulation_output = year_data,
        year              = yr,
        weight_col        = weight_col
      )

      # Disease-specific PAFs from GBD RRs × simulated prevalence
      pafs_yr <- .pafs_for_year(year_data, yr)

      # Ratio-rescaled attributable costs
      costs_out <- calculate_obesity_costs_age_sex(
        paf_by_outcome         = pafs_yr$paf_by_outcome,
        outcome_mapping        = outcome_mapping,
        costs_dro              = grown$costs,
        reference_paf_by_group = reference_used,
        exclude_disease_groups = exclude_disease_groups
      )

      national <- costs_out$costs_by_group
      if (isTRUE(include_direct_costs)) {
        direct_factor <- .exposure_prevalence(prev_yr) /
          base$exposure_prevalence
        national <- dplyr::bind_rows(
          national,
          .direct_rows(grown$costs, direct_factor)
        )
      }
      national <- national |>
        dplyr::mutate(year = yr, unit_growth_factor = grown$unit_growth_factor)

      if (!with_sex) {
        return(list(national = national, by_sex = NULL))
      }

      # Sex allocation weights: PAF × weighted population for PAF-based
      # groups; weighted exposed population for direct costs.
      sex_weights <- lapply(sexes, function(sx) {
        prev_sex <- extract_simulated_obesity_prevalence_by_age_sex(
          simulation_output = year_data[year_data$sex == sx, , drop = FALSE],
          year       = yr,
          weight_col = weight_col
        )

        pafs_sex <- .pafs_for_year(
          year_data[year_data$sex == sx, , drop = FALSE], yr
        )

        paf_sex <- calculate_obesity_costs_age_sex(
          paf_by_outcome         = pafs_sex$paf_by_outcome,
          outcome_mapping        = outcome_mapping,
          costs_dro              = grown$costs,
          reference_paf_by_group = reference_used,
          exclude_disease_groups = exclude_disease_groups
        )$costs_by_group

        population_sex <- sum(prev_sex$population_n)
        exposed_sex <- population_sex * .exposure_prevalence(prev_sex)

        weights <- data.frame(
          disease_group = paf_sex$disease_group,
          sex = sx,
          allocation_weight = paf_sex$paf_obesity * population_sex,
          stringsAsFactors = FALSE
        )

        if (isTRUE(include_direct_costs)) {
          weights <- rbind(
            weights,
            data.frame(
              disease_group = direct_cost_group,
              sex = sx,
              allocation_weight = exposed_sex,
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

    list(
      by_year_disease = dplyr::bind_rows(lapply(year_results, `[[`, "national")),
      by_sex_disease  = dplyr::bind_rows(lapply(year_results, `[[`, "by_sex"))
    )
  }

  pooled <- .costs_for_data(simulation_output)
  by_year_disease <- pooled$by_year_disease
  by_sex_disease <- pooled$by_sex_disease

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
  if (isTRUE(by_simulation) && "simulation" %in% names(simulation_output)) {
    simulation_ids <- sort(unique(simulation_output$simulation))
    split_output <- split(simulation_output, simulation_output$simulation)

    totals_by_simulation <- lapply(simulation_ids, function(sim_id) {
      sim_costs <- .costs_for_data(
        split_output[[as.character(sim_id)]],
        with_sex = FALSE
      )
      sim_costs$by_year_disease |>
        dplyr::group_by(year) |>
        dplyr::summarise(
          total_keur_2018_constant = sum(total_obesity, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(simulation = sim_id, .before = 1L)
    }) |>
      dplyr::bind_rows()

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
