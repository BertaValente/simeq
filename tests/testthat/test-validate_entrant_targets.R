make_entrant_targets_for_validation <- function() {
  entrant_targets <- expand.grid(
    entry_year = 2021:2023,
    sex = c(
      "men",
      "women"
    ),
    stringsAsFactors = FALSE
  )

  entrant_targets$entrant_population <- c(
    49000,
    47000,
    50000,
    48000,
    51000,
    49000
  )

  entrant_targets
}

test_that(
  "simeq_validate_entrant_targets accepts complete valid targets",
  {
    entrant_targets <- make_entrant_targets_for_validation()

    expect_invisible(
      simeq_validate_entrant_targets(
        entrant_targets = entrant_targets,
        start_year = 2021L,
        end_year = 2023L
      )
    )
  }
)

test_that(
  "simeq_validate_entrant_targets requires expected columns",
  {
    entrant_targets <- make_entrant_targets_for_validation()

    entrant_targets$entrant_population <- NULL

    expect_error(
      simeq_validate_entrant_targets(
        entrant_targets = entrant_targets,
        start_year = 2021L,
        end_year = 2023L
      ),
      "`entrant_targets` is missing required columns: entrant_population"
    )
  }
)

test_that(
  "simeq_validate_entrant_targets requires positive populations",
  {
    entrant_targets <- make_entrant_targets_for_validation()

    entrant_targets$entrant_population[1] <- 0

    expect_error(
      simeq_validate_entrant_targets(
        entrant_targets = entrant_targets,
        start_year = 2021L,
        end_year = 2023L
      ),
      "`entrant_population` must contain positive, finite values"
    )
  }
)

test_that(
  "simeq_validate_entrant_targets requires both sex groups every year",
  {
    entrant_targets <- make_entrant_targets_for_validation()

    entrant_targets <- entrant_targets[
      !(
        entrant_targets$entry_year == 2022L &
          entrant_targets$sex == "women"
      ),
      ,
      drop = FALSE
    ]

    expect_error(
      simeq_validate_entrant_targets(
        entrant_targets = entrant_targets,
        start_year = 2021L,
        end_year = 2023L
      ),
      "`entrant_targets` must cover every year-sex combination"
    )
  }
)

test_that(
  "simeq_validate_entrant_targets rejects duplicate year-sex rows",
  {
    entrant_targets <- make_entrant_targets_for_validation()

    entrant_targets <- rbind(
      entrant_targets,
      entrant_targets[1, ]
    )

    expect_error(
      simeq_validate_entrant_targets(
        entrant_targets = entrant_targets,
        start_year = 2021L,
        end_year = 2023L
      ),
      "`entrant_targets` must contain one record per year-sex combination"
    )
  }
)

test_that(
  "simeq_validate_entrant_targets rejects years outside the range",
  {
    entrant_targets <- make_entrant_targets_for_validation()

    entrant_targets$entry_year[1] <- 2020L

    expect_error(
      simeq_validate_entrant_targets(
        entrant_targets = entrant_targets,
        start_year = 2021L,
        end_year = 2023L
      ),
      "`entrant_targets` must cover only years from 2021 to 2023"
    )
  }
)

test_that(
  "simeq_validate_entrant_targets validates requested year bounds",
  {
    entrant_targets <- make_entrant_targets_for_validation()

    expect_error(
      simeq_validate_entrant_targets(
        entrant_targets = entrant_targets,
        start_year = 2023L,
        end_year = 2021L
      ),
      "`end_year` must not be earlier than `start_year`"
    )
  }
)
