test_that(
  "ponte_plot_qaly_trajectory returns a QALY trajectory plot",
  {
    skip_if_not_installed("ggplot2")

    qaly_results <- list(
      by_year_obesity = data.frame(
        year = c(2020L, 2021L, 2020L, 2021L),
        sex = c("women", "women", "women", "women"),
        education_level = c("high", "high", "high", "high"),
        obesity_status = c(
          "no_obesity",
          "no_obesity",
          "obesity",
          "obesity"
        ),
        mean_qaly_per_person = c(
          0.987,
          0.986,
          0.940,
          0.938
        ),
        qaly_lo = c(
          0.980,
          0.979,
          0.930,
          0.928
        ),
        qaly_hi = c(
          0.993,
          0.992,
          0.950,
          0.948
        ),
        stringsAsFactors = FALSE
      )
    )

    results <- ponte_plot_qaly_trajectory(
      qaly_results = qaly_results
    )

    expect_s3_class(
      results,
      "ggplot"
    )
  }
)
