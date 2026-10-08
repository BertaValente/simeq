testthat::test_that(
  "published 2018 direct costs reconcile to the reported total",
  {
    costs <- obesity_direct_costs_2018()

    testthat::expect_equal(
      sum(costs$total),
      1147530
    )  # # Disease-group values displayed in Table 7 sum to 1,147,530 thousand EUR;
    # the paper's reported total is 1,147,529 thousand EUR, a 1-thousand-EUR
    # rounding difference.

    testthat::expect_true(
      all(c(
        "disease_group",
        "inpatient",
        "outpatient",
        "pharmacological",
        "total"
      ) %in% names(costs))
    )
  }
)

testthat::test_that(
  "published PAFs contain one valid value per cost disease group",
  {
    pafs <- obesity_overweight_pafs()

    testthat::expect_false(anyDuplicated(pafs$disease_group) > 0L)

    testthat::expect_true(
      all(is.finite(pafs$paf_ow_ob))
    )

    testthat::expect_true(
      all(pafs$paf_ow_ob >= 0 & pafs$paf_ow_ob <= 1)
    )
  }
)

testthat::test_that(
  "observed 2018 prevalence has valid sex-specific proportions",
  {
    prevalence <- obesity_prevalence_2018()

    testthat::expect_equal(
      prevalence$sex,
      c("Males", "Females")
    )

    testthat::expect_equal(
      prevalence$p_obesity,
      c(0.249, 0.321)
    )
  }
)

testthat::test_that(
  "simulated prevalence extraction converts percent to proportion",
  {
    simulated <- tibble::tibble(
      year = c(2025L, 2025L),
      sex = c("men", "women"),
      obesity_prevalence_pct = c(28.5, 33.2)
    )

    extracted <- extract_simulated_obesity_prevalence(
      obesity_projection_sex = simulated,
      year = 2025L
    )

    testthat::expect_equal(
      extracted$p_obesity,
      c(0.332, 0.285)
    )

    testthat::expect_equal(
      extracted$sex,
      c("Females", "Males")
    )
  }
)

testthat::test_that(
  "medical cost projection reproduces its anchor cost",
  {
    prevalence_projection <- tibble::tibble(
      year = 2025:2027,
      prev_obesity = c(0.30, 0.31, 0.32)
    )

    cpi <- tibble::tibble(
      year = 2025:2027,
      cpi_index = c(120, 123.6, 127.308)
    )

    projection <- project_medical_costs(
      prevalence_projection = prevalence_projection,
      anchor_cost = 1000000,
      anchor_year = 2025L,
      cpi = cpi
    )

    anchor_row <- projection |>
      dplyr::filter(year == 2025L)

    testthat::expect_equal(
      anchor_row$cost_2025_prices,
      1000000
    )

    testthat::expect_equal(
      anchor_row$cost_nominal,
      1000000
    )
  }
)

testthat::test_that(
  "cost-table formatting returns requested reporting years",
  {
    projection <- tibble::tibble(
      year = 2025:2027,
      prev_obesity = c(0.30, 0.31, 0.32),
      cost_2025_prices = c(1000000, 1033333.33, 1066666.67),
      cost_nominal = c(1000000, 1064333.33, 1131133.34)
    )

    output <- format_obesity_cost_table(
      medical_cost_projection = projection,
      years = c(2025L, 2027L)
    )

    testthat::expect_equal(
      output$year,
      c(2025L, 2027L)
    )

    testthat::expect_equal(
      output$obesity_prevalence_pct,
      c(30, 32)
    )
  }
)
