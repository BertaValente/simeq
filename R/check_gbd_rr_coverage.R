#' Check GBD relative-risk coverage for cost outcomes
#'
#' Checks whether every outcome in an obesity cost mapping has a corresponding
#' outcome in a GBD BMI relative-risk matrix.
#'
#' @param rr_matrix A data frame returned by [read_gbd_bmi_rr()].
#' @param outcome_mapping A data frame returned by
#'   [obesity_cost_outcome_mapping()].
#'
#' @return A tibble of mapped outcomes without a matching GBD relative-risk
#'   outcome. A zero-row tibble indicates complete coverage.
#' @export
check_gbd_rr_coverage <- function(
    rr_matrix,
    outcome_mapping
) {
  required_rr <- "disease"
  required_mapping <- c("outcome_pattern", "disease_group")

  missing_rr <- setdiff(required_rr, names(rr_matrix))
  missing_mapping <- setdiff(required_mapping, names(outcome_mapping))

  if (length(missing_rr) > 0L) {
    stop(
      "rr_matrix is missing required column(s): ",
      paste(missing_rr, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_mapping) > 0L) {
    stop(
      "outcome_mapping is missing required column(s): ",
      paste(missing_mapping, collapse = ", "),
      call. = FALSE
    )
  }

  mapped_outcomes <- outcome_mapping |>
    dplyr::transmute(
      outcome = as.character(outcome_pattern),
      disease_group
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      outcome_lc = tolower(outcome)
    )

  gbd_outcomes <- rr_matrix |>
    dplyr::transmute(
      outcome_lc = tolower(as.character(disease))
    ) |>
    dplyr::distinct()

  mapped_outcomes |>
    dplyr::anti_join(
      gbd_outcomes,
      by = "outcome_lc"
    ) |>
    dplyr::select(outcome, disease_group)
}
