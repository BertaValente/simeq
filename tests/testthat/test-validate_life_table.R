make_life_table_for_validation <- function() {
  life_table <- expand.grid(
    age = 20:84,
    sex = c(
      "men",
      "women"
    ),
    stringsAsFactors = FALSE
  )

  life_table$qx <- ifelse(
    life_table$sex == "men",
    0.02,
    0.01
  )

  life_table
}

test_that(
  "simeq_validate_life_table accepts a complete valid life table",
  {
    life_table <- make_life_table_for_validation()

    expect_invisible(
      simeq_validate_life_table(life_table)
    )
  }
)

test_that(
  "simeq_validate_life_table requires age sex and qx",
  {
    life_table <- make_life_table_for_validation()

    life_table$qx <- NULL

    expect_error(
      simeq_validate_life_table(life_table),
      "`life_table` is missing required columns: qx"
    )
  }
)

test_that(
  "simeq_validate_life_table requires valid qx probabilities",
  {
    life_table <- make_life_table_for_validation()

    life_table$qx[1] <- -0.01

    expect_error(
      simeq_validate_life_table(life_table),
      "`qx` must contain finite probabilities from 0 to 1"
    )

    life_table <- make_life_table_for_validation()

    life_table$qx[1] <- 1.01

    expect_error(
      simeq_validate_life_table(life_table),
      "`qx` must contain finite probabilities from 0 to 1"
    )
  }
)

test_that(
  "simeq_validate_life_table requires both sex groups",
  {
    life_table <- make_life_table_for_validation()

    life_table <- life_table[
      life_table$sex == "women",
      ,
      drop = FALSE
    ]

    expect_error(
      simeq_validate_life_table(life_table),
      "`life_table` must cover every age-sex combination"
    )
  }
)

test_that(
  "simeq_validate_life_table requires complete age coverage",
  {
    life_table <- make_life_table_for_validation()

    life_table <- life_table[
      !(
        life_table$age == 50L &
          life_table$sex == "men"
      ),
      ,
      drop = FALSE
    ]

    expect_error(
      simeq_validate_life_table(life_table),
      "`life_table` must cover every age-sex combination"
    )
  }
)

test_that(
  "simeq_validate_life_table rejects duplicate age-sex records",
  {
    life_table <- make_life_table_for_validation()

    life_table <- rbind(
      life_table,
      life_table[1, ]
    )

    expect_error(
      simeq_validate_life_table(life_table),
      "`life_table` must contain one record per age-sex combination"
    )
  }
)

test_that(
  "simeq_validate_life_table rejects ages outside model bounds",
  {
    life_table <- make_life_table_for_validation()

    life_table$age[1] <- 19L

    expect_error(
      simeq_validate_life_table(life_table),
      "`life_table` must cover only ages from 20 to 84"
    )
  }
)
