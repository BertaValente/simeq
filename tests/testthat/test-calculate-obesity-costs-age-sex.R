test_that(
  "calculate_obesity_costs_age_sex rescales published costs",
  {
    paf_by_outcome <- tibble::tibble(
      outcome = "Example disease",
      paf_obesity = 0.20
    )

    outcome_mapping <- tibble::tibble(
      outcome_pattern = "Example disease",
      disease_group = "example_group"
    )

    costs_dro <- tibble::tibble(
      disease_group = "example_group",
      inpatient = 100,
      outpatient = 200,
      pharmacological = 300,
      total = 600
    )

    paf_ow_ob <- tibble::tibble(
      disease_group = "example_group",
      paf_ow_ob = 0.40
    )

    result <- calculate_obesity_costs_age_sex(
      paf_by_outcome = paf_by_outcome,
      outcome_mapping = outcome_mapping,
      costs_dro = costs_dro,
      paf_ow_ob = paf_ow_ob
    )

    expect_named(
      result,
      c("costs_by_group", "paf_by_group", "total_costs")
    )

    expect_equal(
      result$costs_by_group$scale_factor_paf,
      0.5
    )

    expect_equal(
      result$costs_by_group$total_obesity,
      300
    )

    expect_equal(
      result$total_costs$total_obesity,
      300
    )
  }
)


test_that(
  "calculate_obesity_costs_age_sex excludes specified disease groups",
  {
    paf_by_outcome <- tibble::tibble(
      outcome = "Example disease",
      paf_obesity = 0.20
    )

    outcome_mapping <- tibble::tibble(
      outcome_pattern = c(
        "Example disease",
        "Unavailable disease"
      ),
      disease_group = c(
        "example_group",
        "depression"
      )
    )

    costs_dro <- tibble::tibble(
      disease_group = c(
        "example_group",
        "depression",
        "obesity_overweight"
      ),
      inpatient = c(100, 10, 110),
      outpatient = c(200, 20, 220),
      pharmacological = c(300, 30, 330),
      total = c(600, 60, 660)
    )

    paf_ow_ob <- tibble::tibble(
      disease_group = c(
        "example_group",
        "depression"
      ),
      paf_ow_ob = c(0.40, 0.20)
    )

    result <- calculate_obesity_costs_age_sex(
      paf_by_outcome = paf_by_outcome,
      outcome_mapping = outcome_mapping,
      costs_dro = costs_dro,
      paf_ow_ob = paf_ow_ob,
      exclude_disease_groups = c(
        "depression",
        "obesity_overweight"
      )
    )

    expect_equal(
      result$costs_by_group$disease_group,
      "example_group"
    )

    expect_false(
      any(
        result$costs_by_group$disease_group %in%
          c("depression", "obesity_overweight")
      )
    )

    expect_equal(
      result$total_costs$total_obesity,
      300
    )
  }
)
