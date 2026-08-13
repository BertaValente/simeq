make_valid_bmi_panel <- function() {
  data.frame(
    year = c(
      2015L,
      2015L,
      2020L,
      2020L
    ),
    sex = c(
      "men",
      "women",
      "men",
      "women"
    ),
    age = c(
      20L,
      20L,
      25L,
      25L
    ),
    education_level2 = c(
      "low_medium",
      "higher",
      "low_medium",
      "higher"
    ),
    survey_weight = c(
      800,
      900,
      850,
      950
    ),
    bmi = c(
      22.1,
      23.8,
      25.4,
      24.7
    )
  )
}

test_that(
  "ponte_validate_bmi_panel accepts a valid BMI panel",
  {
    expect_invisible(
      ponte_validate_bmi_panel(
        bmi_panel = make_valid_bmi_panel()
      )
    )
  }
)

test_that(
  "ponte_validate_bmi_panel requires all input columns",
  {
    bmi_panel <- make_valid_bmi_panel()

    bmi_panel$bmi <- NULL

    expect_error(
      ponte_validate_bmi_panel(
        bmi_panel = bmi_panel
      ),
      "`bmi_panel` is missing required columns: bmi"
    )
  }
)

test_that(
  "ponte_validate_bmi_panel requires men and women",
  {
    bmi_panel <- make_valid_bmi_panel()

    bmi_panel$sex <- "men"

    expect_error(
      ponte_validate_bmi_panel(
        bmi_panel = bmi_panel
      ),
      "`bmi_panel$sex` must contain both `men` and `women`",
      fixed = TRUE
    )
  }
)
test_that(
  "ponte_validate_bmi_panel rejects ages outside the model range",
  {
    bmi_panel <- make_valid_bmi_panel()

    bmi_panel$age[1L] <- 19L

    expect_error(
      ponte_validate_bmi_panel(
        bmi_panel = bmi_panel
      ),
      "`bmi_panel$age` must contain only integer values from 20 to 84",
      fixed = TRUE
    )
  }
)

test_that(
  "ponte_validate_bmi_panel requires positive survey weights",
  {
    bmi_panel <- make_valid_bmi_panel()

    bmi_panel$survey_weight[1L] <- 0

    expect_error(
      ponte_validate_bmi_panel(
        bmi_panel = bmi_panel
      ),
      "`bmi_panel$survey_weight` must contain only positive finite values",
      fixed = TRUE
    )
  }
)

test_that(
  "ponte_validate_bmi_panel requires positive BMI values",
  {
    bmi_panel <- make_valid_bmi_panel()

    bmi_panel$bmi[1L] <- 0

    expect_error(
      ponte_validate_bmi_panel(
        bmi_panel = bmi_panel
      ),
      "`bmi_panel$bmi` must contain only positive finite values",
      fixed = TRUE
    )
  }
)
