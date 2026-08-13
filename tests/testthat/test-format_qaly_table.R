test_that(
  "ponte_format_qaly_table formats QALY trajectories and intervals",
  {
    qaly_results <- list(
      by_year_obesity = data.frame(
        year = 2020L,
        sex = "women",
        education_level = "high",
        obesity_status = "obesity",
        mean_qaly_per_person = 0.91234,
        qaly_lo = 0.90123,
        qaly_hi = 0.92345,
        stringsAsFactors = FALSE
      )
    )

    results <- ponte_format_qaly_table(
      qaly_results = qaly_results
    )

    expect_equal(
      names(results),
      c(
        "year",
        "sex",
        "education_level",
        "obesity_status",
        "qaly_per_person_95ci"
      )
    )

    expect_equal(
      results$qaly_per_person_95ci,
      "0.912 (0.901\u20130.923)"
    )
  }
)
