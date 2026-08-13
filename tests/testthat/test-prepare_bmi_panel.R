make_bmi_panel_for_preparation <- function() {
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
      24L,
      25L,
      84L
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
    ),
    source_record = c(
      "a",
      "b",
      "c",
      "d"
    ),
    stringsAsFactors = FALSE
  )
}

test_that(
  "ponte_prepare_bmi_panel retains original columns",
  {
    bmi_panel <- make_bmi_panel_for_preparation()

    prepared_panel <- ponte_prepare_bmi_panel(
      bmi_panel = bmi_panel
    )

    expect_true(
      all(
        names(bmi_panel) %in%
          names(prepared_panel)
      )
    )

    expect_identical(
      prepared_panel$source_record,
      bmi_panel$source_record
    )

    expect_identical(
      nrow(prepared_panel),
      nrow(bmi_panel)
    )
  }
)

test_that(
  "ponte_prepare_bmi_panel appends expected age groups",
  {
    prepared_panel <- ponte_prepare_bmi_panel(
      bmi_panel = make_bmi_panel_for_preparation()
    )

    expect_identical(
      prepared_panel$age_group,
      c(
        "20_24",
        "20_24",
        "25_29",
        "80_84"
      )
    )
  }
)

test_that(
  "ponte_prepare_bmi_panel validates the input panel",
  {
    bmi_panel <- make_bmi_panel_for_preparation()

    bmi_panel$bmi[1L] <- NA_real_

    expect_error(
      ponte_prepare_bmi_panel(
        bmi_panel = bmi_panel
      ),
      "`bmi_panel$bmi` must contain only positive finite values.",
      fixed = TRUE
    )
  }
)
