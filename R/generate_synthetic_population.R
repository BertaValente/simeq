#' Generate a fully synthetic SIMEQ-PT baseline population
#'
#' Generates new simulation agents from POND2-standardized survey weights.
#' The observed NHS records and their survey weights are used only to estimate
#' distributions; neither is retained in the resulting synthetic population.
#'
#' @param population Baseline NHS data with `sex`, `age`, `education_level2`,
#'   `bmi`, and `survey_weight`.
#' @param n_agents Number of synthetic individuals to generate.
#' @param seed Optional integer random seed.
#' @param bmi_jitter_sd Standard deviation of BMI jitter in kg/m^2.
#'
#' @return A tibble of synthetic simulation agents.
#' @export
simeq_generate_synthetic_population <- function(
    population,
    n_agents = 100000L,
    seed = NULL,
    bmi_jitter_sd = 0.10
) {
  required_columns <- c(
    "sex",
    "age",
    "education_level2",
    "bmi",
    "survey_weight"
  )

  missing_columns <- setdiff(required_columns, names(population))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  donors <- population |>
    dplyr::transmute(
      sex = as.character(sex),
      age = as.integer(age),
      education_level2 = as.character(education_level2),
      bmi = as.numeric(bmi),
      survey_weight = as.numeric(survey_weight)
    ) |>
    dplyr::filter(
      !is.na(sex),
      !is.na(age),
      !is.na(education_level2),
      !is.na(bmi),
      !is.na(survey_weight),
      survey_weight > 0,
      age >= 20L,
      age <= 84L,
      bmi > 10,
      bmi < 80
    ) |>
    dplyr::mutate(
      sex_age_key = paste(sex, age, sep = "|"),
      sex_age_education_key = paste(sex, age, education_level2, sep = "|")
    )

  if (nrow(donors) == 0L) {
    stop("No eligible baseline donor records remain.", call. = FALSE)
  }

  sample_one <- function(values, weights) {
    keep <- !is.na(values) & !is.na(weights) & is.finite(weights) & weights > 0
    values <- values[keep]
    weights <- weights[keep]

    if (length(values) == 0L) {
      stop("No valid weighted donor values are available.", call. = FALSE)
    }

    sample.int(
      n = length(values),
      size = 1L,
      replace = TRUE,
      prob = weights
    ) |>
      (
        function(index) values[index]
      )()
  }

  demographic_distribution <- donors |>
    dplyr::group_by(sex, age, sex_age_key) |>
    dplyr::summarise(
      weight = sum(survey_weight),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(weight), weight > 0)

  demographic_index <- sample.int(
    n = nrow(demographic_distribution),
    size = n_agents,
    replace = TRUE,
    prob = demographic_distribution$weight
  )

  synthetic_population <- demographic_distribution |>
    dplyr::slice(demographic_index) |>
    dplyr::transmute(
      id = seq_len(n_agents),
      sex = sex,
      age = age,
      sex_age_key = sex_age_key
    )

  education_distribution <- donors |>
    dplyr::group_by(sex_age_key, education_level2) |>
    dplyr::summarise(
      weight = sum(survey_weight),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(weight), weight > 0)

  education_by_sex_age <- split(
    education_distribution,
    education_distribution$sex_age_key
  )

  all_education <- education_distribution

  synthetic_population$education_level2 <- vapply(
    synthetic_population$sex_age_key,
    FUN = function(key) {
      candidates <- education_by_sex_age[[key]]

      if (is.null(candidates) || nrow(candidates) == 0L) {
        candidates <- all_education
      }

      as.character(sample_one(
        values = candidates$education_level2,
        weights = candidates$weight
      ))
    },
    FUN.VALUE = character(1)
  )

  synthetic_population$sex_age_education_key <- paste(
    synthetic_population$sex,
    synthetic_population$age,
    synthetic_population$education_level2,
    sep = "|"
  )

  bmi_by_stratum <- split(donors, donors$sex_age_education_key)
  bmi_by_sex_age <- split(donors, donors$sex_age_key)

  synthetic_population$bmi <- vapply(
    seq_len(nrow(synthetic_population)),
    FUN = function(i) {
      bmi_candidates <- bmi_by_stratum[[
        synthetic_population$sex_age_education_key[i]
      ]]

      if (is.null(bmi_candidates) || nrow(bmi_candidates) == 0L) {
        bmi_candidates <- bmi_by_sex_age[[
          synthetic_population$sex_age_key[i]
        ]]
      }

      if (is.null(bmi_candidates) || nrow(bmi_candidates) == 0L) {
        bmi_candidates <- donors
      }

      sampled_bmi <- sample_one(
        values = bmi_candidates$bmi,
        weights = bmi_candidates$survey_weight
      )

      pmax(
        10,
        as.numeric(sampled_bmi) + stats::rnorm(
          n = 1L,
          mean = 0,
          sd = bmi_jitter_sd
        )
      )
    },
    FUN.VALUE = numeric(1)
  )

  represented_population <- sum(donors$survey_weight)

  synthetic_population |>
    dplyr::mutate(
      person_id = paste0(
        "synthetic_",
        seq_len(n_agents)
      ),
      education_level = education_level2,
      survey_weight = represented_population / n_agents,
      age_group = cut(
        age,
        breaks = seq(20, 85, by = 5),
        right = FALSE,
        include.lowest = TRUE
      ),
      alive = TRUE
    ) |>
    dplyr::select(
      person_id,
      age,
      sex,
      education_level,
      education_level2,
      survey_weight,
      bmi,
      age_group,
      alive
    )
}
