#' Project obesity-attributable medical costs
#'
#' Projects obesity-attributable direct medical costs from an anchor year
#' using annual simulated obesity prevalence and a CPI series.
#'
#' @param prevalence_projection A data frame containing annual obesity
#'   prevalence. It must contain `year` and `prev_obesity`, where prevalence
#'   is expressed as a proportion from 0 to 1.
#' @param anchor_cost Total obesity-attributable direct medical cost in the
#'   anchor year.
#' @param anchor_year Year corresponding to `anchor_cost`.
#' @param cpi A data frame containing `year` and `cpi_index`.
#'
#' @return A data frame of constant-price and nominal medical-cost projections.
#' @export
project_medical_costs <- function(
    prevalence_projection,
    anchor_cost,
    anchor_year,
    cpi
) {
  required_prevalence <- c("year", "prev_obesity")
  required_cpi <- c("year", "cpi_index")

  missing_prevalence <- setdiff(
    required_prevalence,
    names(prevalence_projection)
  )

  missing_cpi <- setdiff(required_cpi, names(cpi))

  if (length(missing_prevalence) > 0L) {
    stop(
      "prevalence_projection is missing required column(s): ",
      paste(missing_prevalence, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_cpi) > 0L) {
    stop(
      "cpi is missing required column(s): ",
      paste(missing_cpi, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(anchor_cost) != 1L ||
      !is.finite(anchor_cost) ||
      anchor_cost < 0) {
    stop(
      "anchor_cost must be one finite non-negative number.",
      call. = FALSE
    )
  }

  if (length(anchor_year) != 1L ||
      !is.finite(anchor_year) ||
      anchor_year != as.integer(anchor_year)) {
    stop(
      "anchor_year must be one whole-number year.",
      call. = FALSE
    )
  }

  prevalence_projection <- prevalence_projection |>
    dplyr::transmute(
      year = as.integer(year),
      prev_obesity = as.numeric(prev_obesity)
    )

  cpi <- cpi |>
    dplyr::transmute(
      year = as.integer(year),
      cpi_index = as.numeric(cpi_index)
    )

  if (anyDuplicated(prevalence_projection$year) > 0L) {
    stop(
      "prevalence_projection must contain no more than one row per year.",
      call. = FALSE
    )
  }

  if (anyDuplicated(cpi$year) > 0L) {
    stop(
      "cpi must contain no more than one row per year.",
      call. = FALSE
    )
  }

  if (any(!is.finite(prevalence_projection$prev_obesity)) ||
      any(prevalence_projection$prev_obesity < 0 |
          prevalence_projection$prev_obesity > 1)) {
    stop(
      "prev_obesity must contain finite proportions between 0 and 1.",
      call. = FALSE
    )
  }

  if (any(!is.finite(cpi$cpi_index)) || any(cpi$cpi_index <= 0)) {
    stop(
      "cpi_index must contain finite values greater than zero.",
      call. = FALSE
    )
  }

  if (!anchor_year %in% prevalence_projection$year) {
    stop(
      "anchor_year is absent from prevalence_projection.",
      call. = FALSE
    )
  }

  if (!anchor_year %in% cpi$year) {
    stop(
      "anchor_year is absent from cpi.",
      call. = FALSE
    )
  }

  prevalence_anchor <- prevalence_projection |>
    dplyr::filter(year == anchor_year) |>
    dplyr::pull(prev_obesity)

  if (length(prevalence_anchor) != 1L || prevalence_anchor <= 0) {
    stop(
      "The anchor-year obesity prevalence must be one value greater than zero.",
      call. = FALSE
    )
  }

  cpi_anchor <- cpi |>
    dplyr::filter(year == anchor_year) |>
    dplyr::pull(cpi_index)

  cost_per_prevalence_unit <- anchor_cost / prevalence_anchor

  projection <- prevalence_projection |>
    dplyr::left_join(cpi, by = "year") |>
    dplyr::arrange(year)

  missing_cpi_years <- projection |>
    dplyr::filter(is.na(cpi_index)) |>
    dplyr::pull(year)

  if (length(missing_cpi_years) > 0L) {
    stop(
      "CPI values are missing for year(s): ",
      paste(missing_cpi_years, collapse = ", "),
      call. = FALSE
    )
  }

  projection |>
    dplyr::mutate(
      cost_2025_prices = cost_per_prevalence_unit * prev_obesity,
      cost_nominal = cost_2025_prices * (cpi_index / cpi_anchor)
    )
}
