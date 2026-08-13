#' Calculate years of life lost
#'
#' Calculates individual and weighted years of life lost (YLL) from
#' PONTE-PT-O microsimulation output.
#'
#' @param simulation_output A data frame returned by
#'   [ponte_run_simulation()].
#' @param ref_age Reference age for YLL calculation.
#' @param min_age Minimum age at death included in YLL calculations.
#' @param max_age Maximum age at death included in YLL calculations.
#'
#' @return A list containing individual-level YLL records and weighted
#'   summaries overall and by death year.
#'
#' @export
ponte_calculate_yll <- function(
    simulation_output,
    ref_age = 75L,
    min_age = 20L,
    max_age = 84L
) {
  required_columns <- c(
    "person_id",
    "simulation",
    "sex",
    "education_level",
    "survey_weight",
    "death_year",
    "age_at_death"
  )

  missing_columns <- setdiff(
    required_columns,
    names(simulation_output)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`simulation_output` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  first_non_missing <- function(x) {
    x_non_missing <- x[!is.na(x)]

    if (length(x_non_missing) == 0L) {
      return(NA)
    }

    x_non_missing[[1]]
  }

  person_keys <- paste(
    simulation_output$person_id,
    simulation_output$simulation,
    sep = "\r"
  )

  unique_person_keys <- unique(person_keys)

  individual_rows <- lapply(
    unique_person_keys,
    function(key) {
      index <- which(person_keys == key)

      death_year <- first_non_missing(
        simulation_output$death_year[index]
      )

      age_at_death <- first_non_missing(
        simulation_output$age_at_death[index]
      )

      died <- !is.na(death_year) &&
        !is.na(age_at_death)

      yll <- if (
        died &&
        age_at_death >= min_age &&
        age_at_death <= max_age
      ) {
        max(ref_age - age_at_death, 0)
      } else {
        0
      }

      data.frame(
        person_id = simulation_output$person_id[index[1]],
        simulation = simulation_output$simulation[index[1]],
        sex = simulation_output$sex[index[1]],
        education_level = simulation_output$education_level[index[1]],
        survey_weight = simulation_output$survey_weight[index[1]],
        death_year = as.integer(death_year),
        age_at_death = as.integer(age_at_death),
        died = died,
        yll = yll,
        stringsAsFactors = FALSE
      )
    }
  )

  individual <- do.call(
    rbind,
    individual_rows
  )

  individual$weighted_yll <- individual$yll *
    individual$survey_weight

  individual$weighted_death <- ifelse(
    individual$died,
    individual$survey_weight,
    0
  )

  overall_sim <- stats::aggregate(
    cbind(
      total_yll_weighted = weighted_yll,
      total_deaths_weighted = weighted_death,
      total_population_weighted = survey_weight
    ) ~ simulation + sex + education_level,
    data = individual,
    FUN = sum
  )

  overall_sim$mean_yll_per_death <- ifelse(
    overall_sim$total_deaths_weighted > 0,
    overall_sim$total_yll_weighted /
      overall_sim$total_deaths_weighted,
    NA_real_
  )

  overall_sim$mean_yll_per_person <- ifelse(
    overall_sim$total_population_weighted > 0,
    overall_sim$total_yll_weighted /
      overall_sim$total_population_weighted,
    NA_real_
  )

  overall_keys <- paste(
    overall_sim$sex,
    overall_sim$education_level,
    sep = "\r"
  )

  overall_rows <- lapply(
    unique(overall_keys),
    function(key) {
      index <- which(overall_keys == key)
      x <- overall_sim[index, , drop = FALSE]

      data.frame(
        sex = x$sex[1],
        education_level = x$education_level[1],
        total_yll_weighted = mean(
          x$total_yll_weighted,
          na.rm = TRUE
        ),
        total_deaths_weighted = mean(
          x$total_deaths_weighted,
          na.rm = TRUE
        ),
        total_population_weighted = mean(
          x$total_population_weighted,
          na.rm = TRUE
        ),
        mean_yll_per_death = mean(
          x$mean_yll_per_death,
          na.rm = TRUE
        ),
        mean_yll_per_person = mean(
          x$mean_yll_per_person,
          na.rm = TRUE
        ),
        yll_lo = as.numeric(
          stats::quantile(
            x$mean_yll_per_person,
            0.025,
            na.rm = TRUE
          )
        ),
        yll_hi = as.numeric(
          stats::quantile(
            x$mean_yll_per_person,
            0.975,
            na.rm = TRUE
          )
        ),
        stringsAsFactors = FALSE
      )
    }
  )

  overall <- do.call(
    rbind,
    overall_rows
  )

  deaths <- individual[
    individual$died &
      !is.na(individual$death_year),
    ,
    drop = FALSE
  ]

  if (nrow(deaths) == 0L) {
    by_year_sim <- data.frame(
      simulation = integer(),
      death_year = integer(),
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric(),
      stringsAsFactors = FALSE
    )

    by_year <- data.frame(
      death_year = integer(),
      sex = character(),
      education_level = character(),
      total_yll_weighted = numeric(),
      total_deaths_weighted = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    by_year_sim <- stats::aggregate(
      cbind(
        total_yll_weighted = weighted_yll,
        total_deaths_weighted = weighted_death
      ) ~ simulation + death_year + sex + education_level,
      data = deaths,
      FUN = sum
    )

    by_year_keys <- paste(
      by_year_sim$death_year,
      by_year_sim$sex,
      by_year_sim$education_level,
      sep = "\r"
    )

    by_year_rows <- lapply(
      unique(by_year_keys),
      function(key) {
        index <- which(by_year_keys == key)

        x <- by_year_sim[
          index,
          ,
          drop = FALSE
        ]

        data.frame(
          death_year = x$death_year[1],
          sex = x$sex[1],
          education_level = x$education_level[1],
          total_yll_weighted = mean(
            x$total_yll_weighted,
            na.rm = TRUE
          ),
          total_deaths_weighted = mean(
            x$total_deaths_weighted,
            na.rm = TRUE
          ),
          stringsAsFactors = FALSE
        )
      }
    )

    by_year <- do.call(
      rbind,
      by_year_rows
    )
  }

  list(
    individual = individual,
    overall_sim = overall_sim,
    overall = overall,
    by_year_sim = by_year_sim,
    by_year = by_year
  )
}
