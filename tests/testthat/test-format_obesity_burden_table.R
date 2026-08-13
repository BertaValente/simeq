test_that(
  "ponte_format_obesity_burden_table formats YLL, YLD and DALY rates",
  {
    burden_results <- list(
      by_year = data.frame(
        year = 2020L,
        sex = "men",
        education_level = "medium",
        yll_obesity_per100k = 123.4,
        yll_obesity_per100k_lo = 100.2,
        yll_obesity_per100k_hi = 150.8,
        yld_obesity_per100k = 5000.0,
        yld_obesity_per100k_lo = 4800.2,
        yld_obesity_per100k_hi = 5200.8,
        daly_obesity_per100k = 5123.4,
        daly_obesity_per100k_lo = 4900.2,
        daly_obesity_per100k_hi = 5350.8,
        stringsAsFactors = FALSE
      )
    )

    results <- ponte_format_obesity_burden_table(
      burden_results = burden_results
    )

    expect_equal(
      names(results),
      c(
        "year",
        "sex",
        "education_level",
        "yll_obesity_per100k_95ci",
        "yld_obesity_per100k_95ci",
        "daly_obesity_per100k_95ci"
      )
    )

    expect_equal(
      results$yll_obesity_per100k_95ci,
      "123.4 (100.2\u2013150.8)"
    )

    expect_equal(
      results$yld_obesity_per100k_95ci,
      "5,000.0 (4,800.2\u20135,200.8)"
    )

    expect_equal(
      results$daly_obesity_per100k_95ci,
      "5,123.4 (4,900.2\u20135,350.8)"
    )
  }
)
