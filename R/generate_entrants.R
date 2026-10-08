#' Generate age-20 entrants for an open cohort
#'
#' Generates new age-20 PONTE-PT-O population records by sampling, with
#' replacement, from historical age 20 to 24 donor records. Sampling is
#' performed separately by sex using donor survey weights. Entrant weights are
#' calibrated to sex-specific annual entrant population targets.
#'
#' @param donor_pool A validated historical entrant donor pool.
#' @param entrant_targets A validated annual entrant-target table.
#' @param entry_year Integer. Calendar year in which entrants join the model.
#' @param simulation Integer. Simulation identifier.
#' @param n_entrants Integer. Number of microdata entrants to generate.
#' @param entry_age Integer. Age assigned to all entrants.
#'
#' @return A data frame containing valid living state records for the new
#'   entrants.
#' @export
#'
#' @examples
#' donor_pool <- data.frame(
#'   sex = c("men", "men", "women", "women"),
#'   age = c(20L, 24L, 21L, 23L),
#'   education_level = c("low", "medium", "medium", "high"),
#'   bmi = c(22.1, 27.5, 23.8, 31.0),
#'   survey_weight = c(800, 1000, 900, 1100)
#' )
#'
#' entrant_targets <- data.frame(
#'   entry_year = c(2021L, 2021L),
#'   sex = c("men", "women"),
#'   entrant_population = c(50000, 48000)
#' )
#'
#' simeq_generate_entrants(
#'   donor_pool = donor_pool,
#'   entrant_targets = entrant_targets,
#'   entry_year = 2021L,
#'   simulation = 1L,
#'   n_entrants = 100L
#' )
simeq_generate_entrants <- function(
    donor_pool,
    entrant_targets,
    entry_year,
    simulation,
    n_entrants,
    entry_age = 20L
) {
  simeq_validate_entrant_donor_pool(
    donor_pool = donor_pool,
    entry_age = entry_age
  )

  if (
    !is.numeric(entry_year) ||
    length(entry_year) != 1L ||
    is.na(entry_year) ||
    !is.finite(entry_year) ||
    entry_year != as.integer(entry_year)
  ) {
    stop(
      "`entry_year` must be one finite integer value.",
      call. = FALSE
    )
  }

  entry_year <- as.integer(
    entry_year
  )

  simeq_validate_entrant_targets(
    entrant_targets = entrant_targets,
    start_year = entry_year,
    end_year = entry_year
  )

  if (
    !is.numeric(simulation) ||
    length(simulation) != 1L ||
    is.na(simulation) ||
    !is.finite(simulation) ||
    simulation != as.integer(simulation) ||
    simulation < 1L
  ) {
    stop(
      "`simulation` must be one positive integer value.",
      call. = FALSE
    )
  }

  simulation <- as.integer(
    simulation
  )

  if (
    !is.numeric(n_entrants) ||
    length(n_entrants) != 1L ||
    is.na(n_entrants) ||
    !is.finite(n_entrants) ||
    n_entrants != as.integer(n_entrants) ||
    n_entrants < 2L
  ) {
    stop(
      "`n_entrants` must be one integer value of at least 2.",
      call. = FALSE
    )
  }

  n_entrants <- as.integer(
    n_entrants
  )

  entrant_targets <- entrant_targets[
    entrant_targets$entry_year == entry_year,
    ,
    drop = FALSE
  ]

  entrant_targets <- entrant_targets[
    match(
      c(
        "men",
        "women"
      ),
      as.character(entrant_targets$sex)
    ),
    ,
    drop = FALSE
  ]

  men_target_weight <- entrant_targets$entrant_population[
    entrant_targets$sex == "men"
  ]

  women_target_weight <- entrant_targets$entrant_population[
    entrant_targets$sex == "women"
  ]

  total_target_weight <- men_target_weight +
    women_target_weight

  n_men <- round(
    n_entrants *
      men_target_weight /
      total_target_weight
  )

  n_men <- max(
    1L,
    min(
      as.integer(n_entrants - 1L),
      as.integer(n_men)
    )
  )

  n_women <- as.integer(
    n_entrants - n_men
  )

  has_weight <- "weight" %in% names(donor_pool)
  has_height <- "height" %in% names(donor_pool)

  sample_one_sex <- function(
    sex_value,
    n_to_sample,
    target_weight
  ) {
    sex_donors <- donor_pool[
      as.character(donor_pool$sex) == sex_value,
      ,
      drop = FALSE
    ]

    sampled_index <- sample.int(
      n = nrow(sex_donors),
      size = n_to_sample,
      replace = TRUE,
      prob = sex_donors$survey_weight
    )

    sampled_donors <- sex_donors[
      sampled_index,
      ,
      drop = FALSE
    ]

    df <- data.frame(
      sex = as.character(sampled_donors$sex),
      education_level = as.character(
        sampled_donors$education_level
      ),
      survey_weight = rep(
        as.numeric(target_weight) / n_to_sample,
        n_to_sample
      ),
      bmi = as.numeric(sampled_donors$bmi),
      stringsAsFactors = FALSE
    )

    if (has_weight) df$weight <- as.numeric(sampled_donors$weight)
    if (has_height) df$height <- as.numeric(sampled_donors$height)

    df
  }

  sampled_entrants <- rbind(
    sample_one_sex(
      sex_value = "men",
      n_to_sample = n_men,
      target_weight = men_target_weight
    ),
    sample_one_sex(
      sex_value = "women",
      n_to_sample = n_women,
      target_weight = women_target_weight
    )
  )

  sampled_entrants <- simeq_prepare_education_level2(
    state = sampled_entrants,
    source = "map_3_to_2"
  )

  entrant_bmi <- sampled_entrants$bmi

  entrants <- data.frame(
    person_id = paste0(
      "entrant_",
      entry_year,
      "_",
      simulation,
      "_",
      seq_len(n_entrants)
    ),
    simulation = rep(
      simulation,
      n_entrants
    ),
    year = rep(
      entry_year,
      n_entrants
    ),
    baseline_year = rep(
      entry_year,
      n_entrants
    ),
    age = rep(
      as.integer(entry_age),
      n_entrants
    ),
    sex = sampled_entrants$sex,
    education_level = sampled_entrants$education_level,
    education_level2 = sampled_entrants$education_level2,
    survey_weight = sampled_entrants$survey_weight,
    bmi = entrant_bmi,
    bmi_category = simeq_bmi_category(
      entrant_bmi
    ),
    obesity = entrant_bmi >= 30,
    alive = rep(
      TRUE,
      n_entrants
    ),
    death_year = rep(
      NA_integer_,
      n_entrants
    ),
    age_at_death = rep(
      NA_integer_,
      n_entrants
    ),
    stringsAsFactors = FALSE
  )

  if (has_weight) entrants$weight <- sampled_entrants$weight
  if (has_height) entrants$height <- sampled_entrants$height
  if ("education_years" %in% names(sampled_entrants)) {
    entrants$education_years <- sampled_entrants$education_years
  }

  entrants
}
