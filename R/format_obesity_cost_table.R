#' Format obesity-attributable medical cost projections
#'
#' Formats selected years from a medical-cost projection into a compact
#' reporting table with costs expressed in millions of euros.
#'
#' @param medical_cost_projection A data frame returned by
#'   `project_medical_costs()`. It must contain `year`, `prev_obesity`,
#'   `cost_2025_prices`, and `cost_nominal`.
#' @param years Integer vector of years to include in the output table.
#'
#' @return A data frame formatted for reporting.
#' @export
format_obesity_cost_table <- function(
    medical_cost_projection,
    years = c(2025L, 2030L, 2035L, 2040L)
) {
  required_columns <- c(
    "year",
    "prev_obesity",
    "cost_2025_prices",
    "cost_nominal"
  )

  missing_columns <- setdiff(
    required_columns,
    names(medical_cost_projection)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "medical_cost_projection is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  years <- as.integer(years)

  if (length(years) == 0L || any(!is.finite(years))) {
    stop(
      "years must contain at least one valid whole-number year.",
      call. = FALSE
    )
  }

  output <- medical_cost_projection |>
    dplyr::filter(year %in% years) |>
    dplyr::arrange(year)

  missing_years <- setdiff(years, output$year)

  if (length(missing_years) > 0L) {
    stop(
      "Projection is missing requested year(s): ",
      paste(missing_years, collapse = ", "),
      call. = FALSE
    )
  }

  output |>
    dplyr::transmute(
      year,
      obesity_prevalence_pct = 100 * prev_obesity,
      cost_constant_2025_eur = cost_2025_prices,
      cost_nominal_eur = cost_nominal,
      cost_constant_2025_million_eur =
        cost_constant_2025_eur / 1e6,
      cost_nominal_million_eur =
        cost_nominal_eur / 1e6
    )
}
