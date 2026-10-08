#' @importFrom stats weighted.mean
calculate_scenario_costs <- function(
    simulation_output,
    base_cost_2018_keur,
    base_high_bmi_prevalence_2018,
    cpi_2026 = 117.22,
    discount_rate = 0.03,
    base_year = 2026L
) {
  required_columns <- c(
    "year",
    "alive",
    "bmi",
    "survey_weight"
  )

  missing_columns <- setdiff(
    required_columns,
    names(simulation_output)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  annual_costs <- simulation_output |>
    dplyr::filter(alive) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      high_bmi_prevalence = weighted.mean(
        bmi >= 25,
        w = survey_weight,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prevalence_scale =
        high_bmi_prevalence /
        base_high_bmi_prevalence_2018,

      cost_constant_2018_eur =
        base_cost_2018_keur *
        1000 *
        prevalence_scale,

      cost_2026_eur_undiscounted =
        cost_constant_2018_eur *
        cpi_2026 /
        100,

      discount_factor = dplyr::if_else(
        year <= base_year,
        1,
        1 / (1 + discount_rate)^(year - base_year)
      ),

      cost_2026_eur_pv =
        cost_2026_eur_undiscounted *
        discount_factor
    )

  overall <- annual_costs |>
    dplyr::summarise(
      cumulative_cost_2026_eur_undiscounted = sum(
        cost_2026_eur_undiscounted,
        na.rm = TRUE
      ),
      cumulative_cost_2026_eur_pv = sum(
        cost_2026_eur_pv,
        na.rm = TRUE
      )
    )

  list(
    by_year = annual_costs,
    overall = overall
  )
}
