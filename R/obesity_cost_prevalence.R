#' Create observed 2018 obesity prevalence input
#'
#' Returns sex-specific obesity prevalence from INSEF 2015, used as the
#' observed prevalence input for the published 2018 cost-of-illness anchor.
#'
#' @return A tibble with sex and obesity prevalence as a proportion.
#' @references Borges M, et al. Disease and Economic Burden of Overweight
#'   and Obesity in Portugal. Obesity Facts. 2025;18:107-120.
#'   doi:10.1159/000541781.
#' @export
obesity_prevalence_2018 <- function() {
  tibble::tibble(
    sex = c("Males", "Females"),
    p_obesity = c(0.249, 0.321)
  )
}

#' Extract simulated sex-specific obesity prevalence
#'
#' Extracts one year's sex-specific obesity prevalence from a simulation
#' summary. The input must contain `year`, `sex`, and
#' `obesity_prevalence_pct`.
#'
#' @param obesity_projection_sex A data frame containing annual simulated
#'   obesity prevalence by sex.
#' @param year Year to extract.
#'
#' @return A tibble with `sex` and `p_obesity`, where prevalence is expressed
#'   as a proportion.
#' @export
extract_simulated_obesity_prevalence <- function(
    obesity_projection_sex,
    year = 2025L
) {
  required_columns <- c(
    "year",
    "sex",
    "obesity_prevalence_pct"
  )

  missing_columns <- setdiff(
    required_columns,
    names(obesity_projection_sex)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "obesity_projection_sex is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  year <- as.integer(year)

  if (length(year) != 1L || !is.finite(year)) {
    stop(
      "year must be one valid whole-number year.",
      call. = FALSE
    )
  }

  prevalence <- obesity_projection_sex |>
    dplyr::filter(year == !!year) |>
    dplyr::transmute(
      sex = dplyr::recode(
        as.character(sex),
        men = "Males",
        women = "Females"
      ),
      p_obesity = as.numeric(obesity_prevalence_pct) / 100
    )

  if (nrow(prevalence) != 2L ||
      !all(c("Males", "Females") %in% prevalence$sex)) {
    stop(
      "The requested year must contain one simulated prevalence value for ",
      "each of 'men' and 'women'.",
      call. = FALSE
    )
  }

  if (any(!is.finite(prevalence$p_obesity)) ||
      any(prevalence$p_obesity < 0 | prevalence$p_obesity > 1)) {
    stop(
      "Simulated obesity prevalence must contain finite proportions ",
      "between 0 and 1.",
      call. = FALSE
    )
  }

  prevalence |>
    dplyr::arrange(sex)
}
