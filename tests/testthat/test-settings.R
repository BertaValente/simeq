test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that(
  "simeq_settings returns valid default settings",
  {
    settings <- simeq_settings()

    expect_s3_class(
      settings,
      "simeq_settings"
    )

    expect_identical(
      settings$initial_year,
      2020L
    )

    expect_identical(
      settings$projection_end_year,
      2040L
    )

    expect_identical(
      settings$entry_age,
      20L
    )

    expect_identical(
      settings$max_age,
      84L
    )

    expect_identical(
      settings$n_simulations,
      100L
    )

    expect_identical(
      settings$entrant_n_per_year,
      1000L
    )

    expect_identical(
      settings$seed,
      1234L
    )
  }
)

test_that(
  "simeq_settings accepts valid custom settings",
  {
    settings <- simeq_settings(
      initial_year = 2025L,
      projection_end_year = 2035L,
      entry_age = 18L,
      max_age = 90L,
      n_simulations = 25L,
      entrant_n_per_year = 500L,
      seed = 42L
    )

    expect_identical(
      settings$initial_year,
      2025L
    )

    expect_identical(
      settings$projection_end_year,
      2035L
    )

    expect_identical(
      settings$entry_age,
      18L
    )

    expect_identical(
      settings$max_age,
      90L
    )

    expect_identical(
      settings$n_simulations,
      25L
    )

    expect_identical(
      settings$entrant_n_per_year,
      500L
    )

    expect_identical(
      settings$seed,
      42L
    )
  }
)

test_that(
  "simeq_settings rejects invalid values",
  {
    expect_error(
      simeq_settings(
        initial_year = 2040L,
        projection_end_year = 2020L
      ),
      "`projection_end_year` must be greater"
    )

    expect_error(
      simeq_settings(
        entry_age = -1L
      ),
      "`entry_age` must be zero or greater"
    )

    expect_error(
      simeq_settings(
        entry_age = 84L,
        max_age = 84L
      ),
      "`max_age` must be greater"
    )

    expect_error(
      simeq_settings(
        n_simulations = 0L
      ),
      "`n_simulations` must be at least 1"
    )

    expect_error(
      simeq_settings(
        entrant_n_per_year = 1L
      ),
      "`entrant_n_per_year` must be at least 2"
    )

    expect_error(
      simeq_settings(
        seed = 1.5
      ),
      "must be single finite integers"
    )
  }
)
