test_that(
  "ponte_calculate_qalys applies norms and BMI utility decrements",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2",
        "person_3"
      ),
      simulation = c(1L, 1L, 1L),
      year = c(2020L, 2020L, 2020L),
      age = c(35L, 35L, 60L),
      sex = c("women", "women", "men"),
      education_level = c("medium", "high", "low"),
      survey_weight = c(100, 100, 100),
      bmi_category = c(
        "normal_weight",
        "obesityclassI",
        "obesity_III"
      ),
      alive = c(TRUE, TRUE, FALSE),
      stringsAsFactors = FALSE
    )

    results <- ponte_calculate_qalys(
      simulation_output = simulation_output
    )

    individual <- results$individual

    expect_equal(nrow(individual), 2L)

    expect_equal(
      individual$utility_with_bmi,
      c(0.944, 0.915),
      tolerance = 1e-10
    )

    expect_equal(
      individual$qaly_weighted,
      c(94.4, 91.5),
      tolerance = 1e-10
    )
  }
)

test_that(
  "ponte_calculate_qalys returns yearly weighted QALYs",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2"
      ),
      simulation = c(1L, 1L),
      year = c(2020L, 2020L),
      age = c(25L, 25L),
      sex = c("men", "men"),
      education_level = c("medium", "medium"),
      survey_weight = c(100, 50),
      bmi_category = c(
        "normal_weight",
        "overweight"
      ),
      alive = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )

    results <- ponte_calculate_qalys(
      simulation_output = simulation_output
    )

    by_year_sim <- results$by_year_sim

    expect_equal(
      by_year_sim$total_qaly_weighted,
      147.05,
      tolerance = 1e-10
    )
  }
)

test_that(
  "ponte_calculate_qalys restricts QALYs to living people aged 20 to 84",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2",
        "person_3",
        "person_4"
      ),
      simulation = c(1L, 1L, 1L, 1L),
      year = c(2020L, 2020L, 2020L, 2020L),
      age = c(19L, 20L, 84L, 85L),
      sex = c("women", "women", "women", "women"),
      education_level = c("high", "high", "high", "high"),
      survey_weight = c(10, 10, 10, 10),
      bmi_category = c(
        "normal_weight",
        "normal_weight",
        "normal_weight",
        "normal_weight"
      ),
      alive = c(TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    )

    results <- ponte_calculate_qalys(
      simulation_output = simulation_output
    )

    expect_equal(nrow(results$individual), 2L)
  }
)

test_that(
  "ponte_calculate_qalys returns QALY trajectories by obesity status",
  {
    simulation_output <- data.frame(
      person_id = c(
        "person_1",
        "person_2"
      ),
      simulation = c(1L, 1L),
      year = c(2020L, 2020L),
      age = c(25L, 25L),
      sex = c("men", "men"),
      education_level = c("medium", "medium"),
      survey_weight = c(100, 50),
      bmi_category = c(
        "normal_weight",
        "obesity_I"
      ),
      alive = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )

    results <- ponte_calculate_qalys(
      simulation_output = simulation_output
    )

    trajectory <- results$by_year_obesity_sim

    normal_weight <- trajectory[
      trajectory$obesity_status == "no_obesity",
      ,
      drop = FALSE
    ]

    obesity <- trajectory[
      trajectory$obesity_status == "obesity",
      ,
      drop = FALSE
    ]

    expect_equal(
      normal_weight$mean_annual_qaly_per_person,
      0.987,
      tolerance = 1e-10
    )

    expect_equal(
      obesity$mean_annual_qaly_per_person,
      0.940,
      tolerance = 1e-10
    )
  }
)
