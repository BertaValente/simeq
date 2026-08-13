make_entrant_donor_pool_for_validation <- function() {
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

test_that(
  "ponte_validate_entrant_donor_pool accepts a valid donor pool",
  {
    donor_pool <- make_entrant_donor_pool_for_validation()

    expect_invisible(
      ponte_validate_entrant_donor_pool(
        donor_pool = donor_pool
      )
    )
  }
)

test_that(
  "ponte_validate_entrant_donor_pool requires expected columns",
  {
    donor_pool <- make_entrant_donor_pool_for_validation()

    donor_pool$bmi <- NULL

    expect_error(
      ponte_validate_entrant_donor_pool(
        donor_pool = donor_pool
      ),
      "`donor_pool` is missing required columns: bmi"
    )
  }
)

test_that(
  "ponte_validate_entrant_donor_pool requires records for both sexes",
  {
    donor_pool <- make_entrant_donor_pool_for_validation()

    donor_pool <- donor_pool[
      donor_pool$sex == "women",
      ,
      drop = FALSE
    ]

    expect_error(
      ponte_validate_entrant_donor_pool(
        donor_pool = donor_pool
      ),
      "`donor_pool` must contain records for both `men` and `women`"
    )
  }
)

test_that(
  "ponte_validate_entrant_donor_pool restricts donor ages",
  {
    donor_pool <- make_entrant_donor_pool_for_validation()

    donor_pool$age[1] <- 25L

    expect_error(
      ponte_validate_entrant_donor_pool(
        donor_pool = donor_pool
      ),
      "`donor_pool` must contain only ages from 20 to 24"
    )
  }
)

test_that(
  "ponte_validate_entrant_donor_pool requires valid education levels",
  {
    donor_pool <- make_entrant_donor_pool_for_validation()

    donor_pool$education_level[1] <- "university"

    expect_error(
      ponte_validate_entrant_donor_pool(
        donor_pool = donor_pool
      ),
      "`donor_pool\\$education_level` must contain only `low`, `medium`, and `high`"
    )
  }
)

test_that(
  "ponte_validate_entrant_donor_pool requires positive BMI values",
  {
    donor_pool <- make_entrant_donor_pool_for_validation()

    donor_pool$bmi[1] <- 0

    expect_error(
      ponte_validate_entrant_donor_pool(
        donor_pool = donor_pool
      ),
      "`donor_pool\\$bmi` must contain positive, finite values"
    )
  }
)

test_that(
  "ponte_validate_entrant_donor_pool requires positive survey weights",
  {
    donor_pool <- make_entrant_donor_pool_for_validation()

    donor_pool$survey_weight[1] <- 0

    expect_error(
      ponte_validate_entrant_donor_pool(
        donor_pool = donor_pool
      ),
      "`donor_pool\\$survey_weight` must contain positive, finite values"
    )
  }
)
