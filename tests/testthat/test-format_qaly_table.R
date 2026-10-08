test_that(
  "simeq_format_qaly_table formats QALY trajectories and intervals",
  {
    qaly_results <- list(
      by_year_obesity = data.frame(
        year = 2020L,
        sex = "women",
        education_level = "high",
        obesity_status = "obesity",
        mean_annual_qaly_per_person = 0.91234,
        qaly_per_person_lo = 0.90123,
        qaly_per_person_hi = 0.92345,
        stringsAsFactors = FALSE
      )
    )

    results <- simeq_format_qaly_table(
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

test_that("simeq_format_qaly_table accepts simeq_calculate_qalys output", {
  simulation_output <- data.frame(
    person_id = c("person_1", "person_2"),
    simulation = c(1L, 2L),
    year = c(2020L, 2020L),
    age = c(35L, 35L),
    sex = c("women", "women"),
    education_level = c("medium", "medium"),
    survey_weight = c(100, 100),
    bmi_category = c("obesity_I", "obesity_I"),
    alive = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  qaly_results <- simeq_calculate_qalys(simulation_output)

  expect_no_error(simeq_format_qaly_table(qaly_results))
  expect_s3_class(simeq_plot_qaly_trajectory(qaly_results), "ggplot")
})
