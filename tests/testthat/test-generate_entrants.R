make_donor_pool_for_entrants <- function() {
  data.frame(
    sex = c(
      "men",
      "men",
      "women",
      "women"
    ),
    age = c(
      20L,
      24L,
      21L,
      23L
    ),
    education_level = c(
      "low",
      "medium",
      "medium",
      "high"
    ),
    bmi = c(
      22.1,
      27.5,
      23.8,
      31.0
    ),
    survey_weight = c(
      800,
      1000,
      900,
      1100
    )
  )
}

make_entrant_targets_for_generation <- function() {
  data.frame(
    entry_year = c(
      2021L,
      2021L
    ),
    sex = c(
      "men",
      "women"
    ),
    entrant_population = c(
      49000,
      48000
    )
  )
}

test_that(
  "ponte_generate_entrants creates valid age-20 living records",
  {
    set.seed(123)

    entrants <- ponte_generate_entrants(
      donor_pool = make_donor_pool_for_entrants(),
      entrant_targets = make_entrant_targets_for_generation(),
      entry_year = 2021L,
      simulation = 1L,
      n_entrants = 100L
    )

    expect_s3_class(
      entrants,
      "data.frame"
    )

    expect_identical(
      nrow(entrants),
      100L
    )

    expect_true(
      all(entrants$age == 20L)
    )

    expect_true(
      all(entrants$year == 2021L)
    )

    expect_true(
      all(entrants$baseline_year == 2021L)
    )

    expect_true(
      all(entrants$simulation == 1L)
    )

    expect_true(
      all(entrants$alive)
    )

    expect_true(
      all(is.na(entrants$death_year))
    )

    expect_true(
      all(is.na(entrants$age_at_death))
    )
  }
)

test_that(
  "ponte_generate_entrants creates unique person identifiers",
  {
    set.seed(123)

    entrants <- ponte_generate_entrants(
      donor_pool = make_donor_pool_for_entrants(),
      entrant_targets = make_entrant_targets_for_generation(),
      entry_year = 2021L,
      simulation = 3L,
      n_entrants = 100L
    )

    expect_identical(
      length(unique(entrants$person_id)),
      100L
    )

    expect_true(
      all(
        grepl(
          "^entrant_2021_3_",
          entrants$person_id
        )
      )
    )
  }
)

test_that(
  "ponte_generate_entrants matches sex-specific target weights",
  {
    set.seed(123)

    entrants <- ponte_generate_entrants(
      donor_pool = make_donor_pool_for_entrants(),
      entrant_targets = make_entrant_targets_for_generation(),
      entry_year = 2021L,
      simulation = 1L,
      n_entrants = 100L
    )

    men_weight <- sum(
      entrants$survey_weight[
        entrants$sex == "men"
      ]
    )

    women_weight <- sum(
      entrants$survey_weight[
        entrants$sex == "women"
      ]
    )

    expect_equal(
      men_weight,
      49000
    )

    expect_equal(
      women_weight,
      48000
    )
  }
)

test_that(
  "ponte_generate_entrants creates valid BMI-derived variables",
  {
    set.seed(123)

    entrants <- ponte_generate_entrants(
      donor_pool = make_donor_pool_for_entrants(),
      entrant_targets = make_entrant_targets_for_generation(),
      entry_year = 2021L,
      simulation = 1L,
      n_entrants = 100L
    )

    expect_identical(
      entrants$bmi_category,
      ponte_bmi_category(
        entrants$bmi
      )
    )

    expect_identical(
      entrants$obesity,
      entrants$bmi >= 30
    )
  }
)

test_that(
  "ponte_generate_entrants requires at least two microdata entrants",
  {
    expect_error(
      ponte_generate_entrants(
        donor_pool = make_donor_pool_for_entrants(),
        entrant_targets = make_entrant_targets_for_generation(),
        entry_year = 2021L,
        simulation = 1L,
        n_entrants = 1L
      ),
      "`n_entrants` must be one integer value of at least 2"
    )
  }
)

test_that(
  "ponte_generate_entrants validates the requested entry year",
  {
    expect_error(
      ponte_generate_entrants(
        donor_pool = make_donor_pool_for_entrants(),
        entrant_targets = make_entrant_targets_for_generation(),
        entry_year = 2022L,
        simulation = 1L,
        n_entrants = 100L
      ),
      "`entrant_targets` must cover only years from 2022 to 2022"
    )
  }
)
