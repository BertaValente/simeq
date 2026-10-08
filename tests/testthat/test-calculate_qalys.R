test_that(
  "simeq_calculate_qalys applies norms and BMI utility decrements",
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

    results <- simeq_calculate_qalys(
      simulation_output = simulation_output,
      half_cycle_deaths = FALSE
    )

    individual <- results$individual

    expect_equal(nrow(individual), 2L)

    # U_h (30-49 band) = 0.919 + correction(30-49)
    # correction = 0.020×0.34 + 0.047×0.13 + 0.072×0.04 + 0.084×0.01 = 0.01663
    # U_h = 0.93563
    # person_1 (normal_weight, medium): 0.93563 + 0.025 + 0.000 = 0.96063
    # person_2 (obesity_I, high):       0.93563 + 0.043 - 0.047 = 0.93163
    expect_equal(
      individual$utility_with_bmi,
      c(0.96063, 0.93163),
      tolerance = 1e-10
    )

    expect_equal(
      individual$qaly_weighted,
      c(96.063, 93.163),
      tolerance = 1e-10
    )
  }
)

test_that(
  "simeq_calculate_qalys returns yearly weighted QALYs",
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

    results <- simeq_calculate_qalys(
      simulation_output = simulation_output
    )

    by_year_sim <- results$by_year_sim

    # U_h (20-29 band) = 0.962 + 0.01097 = 0.97297
    # person_1 (normal_weight, w=100): util = 0.97297 + 0.025 + 0 = 0.99797
    # person_2 (overweight,   w= 50): util = 0.97297 + 0.025 - 0.020 = 0.97797
    # total_qaly = 0.99797×100 + 0.97797×50 = 148.6955
    expect_equal(
      by_year_sim$total_qaly_weighted,
      148.6955,
      tolerance = 1e-10
    )
  }
)

test_that(
  "simeq_calculate_qalys restricts QALYs to living people aged 20 to 84",
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

    results <- simeq_calculate_qalys(
      simulation_output = simulation_output
    )

    expect_equal(nrow(results$individual), 2L)
  }
)

test_that(
  "simeq_calculate_qalys returns QALY trajectories by obesity status",
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

    results <- simeq_calculate_qalys(
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

    # U_h (20-29 band) = 0.97297
    # normal_weight (medium): 0.97297 + 0.025 + 0 = 0.99797
    # obesity_I (medium):     0.97297 + 0.025 - 0.047 = 0.95097
    expect_equal(
      normal_weight$mean_annual_qaly_per_person,
      0.99797,
      tolerance = 1e-10
    )

    expect_equal(
      obesity$mean_annual_qaly_per_person,
      0.95097,
      tolerance = 1e-10
    )
  }
)

test_that(
  "simeq_calculate_qalys gives decedents half a cycle in their death year",
  {
    simulation_output <- data.frame(
      person_id = c("person_1", "person_2"),
      simulation = c(1L, 1L),
      year = c(2020L, 2020L),
      age = c(35L, 60L),
      sex = c("women", "men"),
      education_level = c("medium", "low"),
      survey_weight = c(100, 100),
      bmi_category = c("normal_weight", "obesity_III"),
      alive = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    )

    results <- simeq_calculate_qalys(simulation_output)
    individual <- results$individual

    expect_equal(individual$person_years, c(1, 0.5))
    # person_2: U_h(50-69) 0.88823 - 0.039 (low) - 0.084 (obesity III)
    expect_equal(individual$qaly_weighted[2], 0.76523 * 100 * 0.5,
                 tolerance = 1e-10)
    expect_equal(individual$qaly_loss_weighted, c(0, 0.084 * 100 * 0.5),
                 tolerance = 1e-10)

    by_year_sim <- results$by_year_sim
    expect_equal(sum(by_year_sim$weighted_population), 100)
    expect_equal(sum(by_year_sim$weighted_person_years), 150)
  }
)

test_that(
  "simeq_calculate_qalys discounts after the base year",
  {
    simulation_output <- data.frame(
      person_id = c("person_1", "person_1"),
      simulation = c(1L, 1L),
      year = c(2020L, 2022L),
      age = c(35L, 37L),
      sex = c("women", "women"),
      education_level = c("medium", "medium"),
      survey_weight = c(100, 100),
      bmi_category = c("normal_weight", "normal_weight"),
      alive = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )

    individual <- simeq_calculate_qalys(
      simulation_output,
      discount_rate = 0.03,
      discount_base_year = 2020L
    )$individual

    expect_equal(individual$discount_factor, c(1, 1 / 1.03^2))
    expect_equal(
      individual$qaly_weighted_discounted,
      individual$qaly_weighted * c(1, 1 / 1.03^2)
    )
  }
)

test_that(
  "simeq_calculate_qalys accepts two-level education",
  {
    simulation_output <- data.frame(
      person_id = c("person_1", "person_2"),
      simulation = c(1L, 1L),
      year = c(2020L, 2020L),
      age = c(35L, 35L),
      sex = c("women", "women"),
      education_level = c("lower", "higher"),
      survey_weight = c(100, 100),
      bmi_category = c("normal_weight", "normal_weight"),
      alive = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )

    individual <- simeq_calculate_qalys(simulation_output)$individual

    expect_equal(
      individual$education_utility_adjustment,
      c((0.848 + 0.912) / 2 - 0.887, 0.930 - 0.887)
    )
  }
)

test_that(
  "custom bmi_decrements replace defaults and feed the back-calculation",
  {
    simulation_output <- data.frame(
      person_id = c("person_1", "person_2"),
      simulation = c(1L, 1L),
      year = c(2020L, 2020L),
      age = c(35L, 35L),
      sex = c("women", "women"),
      education_level = c("medium", "medium"),
      survey_weight = c(100, 100),
      bmi_category = c("normal_weight", "obesity_I"),
      alive = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )

    default_ind <- simeq_calculate_qalys(simulation_output)$individual
    custom_ind <- simeq_calculate_qalys(
      simulation_output,
      bmi_decrements = c(obesity_I = -0.077)
    )$individual

    expect_equal(custom_ind$bmi_utility_decrement, c(0, -0.077))
    expect_gt(custom_ind$utility_with_bmi[1], default_ind$utility_with_bmi[1])

    expect_error(
      simeq_calculate_qalys(simulation_output, bmi_decrements = c(obese = -0.1)),
      "bmi_decrements"
    )
    expect_error(
      simeq_calculate_qalys(simulation_output, bmi_decrements = c(obesity_I = 0.1)),
      "negative"
    )
  }
)
