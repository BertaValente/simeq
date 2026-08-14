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




#' Extract simulated obesity prevalence by sex and GBD age band
#'
#' Summarises a microsimulation output into weighted obesity prevalence and
#' represented population counts by sex and five-year age bands. The function
#' is designed for PONTE-PT simulations covering adults aged 20 to 84 years.
#'
#' @param simulation_output A data frame containing individual-level simulation
#'   output.
#' @param year The simulation year to extract.
#' @param weight_col Name of the population-weight column.
#'
#' @return A tibble with `year`, `sex`, `age_band`, `population_n`, and
#'   `p_obesity`. Results are limited to living people aged 20 to 84 years.
#' @export
extract_simulated_obesity_prevalence_by_age_sex <- function(
    simulation_output,
    year = 2025L,
    weight_col = "surveyweight"
) {
  required_columns <- c(
    "year",
    "age",
    "sex",
    "obesity",
    "alive",
    weight_col
  )

  missing_columns <- setdiff(required_columns, names(simulation_output))

  if (length(missing_columns) > 0L) {
    stop(
      "simulation_output is missing required column(s): ",
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

  age_band_levels <- c(
    "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79",
    "80-84"
  )

  weight_symbol <- rlang::sym(weight_col)

  prevalence <- simulation_output |>
    dplyr::filter(
      year == !!year,
      alive,
      age >= 20L,
      age <= 84L,
      !is.na(age),
      !is.na(sex),
      !is.na(obesity),
      !is.na(!!weight_symbol),
      !!weight_symbol > 0
    ) |>
    dplyr::mutate(
      sex = dplyr::recode(
        as.character(sex),
        men = "Males",
        women = "Females"
      ),
      age_band = paste0(
        ((as.integer(age) - 20L) %/% 5L) * 5L + 20L,
        "-",
        ((as.integer(age) - 20L) %/% 5L) * 5L + 24L
      ),
      obesity = as.numeric(obesity)
    ) |>
    dplyr::group_by(sex, age_band) |>
    dplyr::summarise(
      population_n = sum(!!weight_symbol, na.rm = TRUE),
      p_obesity = stats::weighted.mean(
        obesity,
        w = !!weight_symbol,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      year = year,
      age_band = factor(age_band, levels = age_band_levels)
    ) |>
    dplyr::arrange(sex, age_band)

  expected_strata <- tidyr::expand_grid(
    sex = c("Males", "Females"),
    age_band = age_band_levels
  )

  missing_strata <- expected_strata |>
    dplyr::anti_join(
      prevalence |>
        dplyr::mutate(age_band = as.character(age_band)),
      by = c("sex", "age_band")
    )

  if (nrow(missing_strata) > 0L) {
    stop(
      "No eligible simulated records were available for: ",
      paste(
        paste(missing_strata$sex, missing_strata$age_band, sep = " / "),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (any(!is.finite(prevalence$population_n)) ||
      any(prevalence$population_n <= 0) ||
      any(!is.finite(prevalence$p_obesity)) ||
      any(prevalence$p_obesity < 0 | prevalence$p_obesity > 1)) {
    stop(
      "Simulated age-sex prevalence contains invalid population weights or ",
      "obesity proportions.",
      call. = FALSE
    )
  }

  prevalence |>
    dplyr::select(year, sex, age_band, population_n, p_obesity)
}
