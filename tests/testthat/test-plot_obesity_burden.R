test_that(
  "ponte_plot_obesity_burden returns a DALY burden plot",
  {
    skip_if_not_installed("ggplot2")

    burden_results <- list(
      by_year = data.frame(
        year = c(2020L, 2021L),
        sex = c("men", "men"),
        education_level = c("medium", "medium"),
        daly_obesity_per100k = c(
          5123.4,
          5200.0
        ),
        daly_obesity_per100k_lo = c(
          4900.2,
          4975.0
        ),
        daly_obesity_per100k_hi = c(
          5350.8,
          5430.0
        ),
        stringsAsFactors = FALSE
      )
    )

    results <- ponte_plot_obesity_burden(
      burden_results = burden_results
    )

    expect_s3_class(
      results,
      "ggplot"
    )
  }
)
