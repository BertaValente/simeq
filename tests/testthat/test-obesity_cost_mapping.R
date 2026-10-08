test_that("obesity_cost_outcome_mapping maps every outcome to a published cost group", {
  mapping <- obesity_cost_outcome_mapping()
  cost_groups <- obesity_direct_costs_2018()$disease_group

  expect_named(mapping, c("outcome_pattern", "disease_group"))
  expect_false(anyDuplicated(mapping$outcome_pattern) > 0L)
  expect_true(all(mapping$disease_group %in% cost_groups))
})

test_that("check_gbd_rr_coverage returns no rows when every outcome has an RR", {
  gaps <- check_gbd_rr_coverage(make_gbd_rr_matrix(), obesity_cost_outcome_mapping())

  expect_equal(nrow(gaps), 0L)
  expect_named(gaps, c("outcome", "disease_group"))
})

test_that("check_gbd_rr_coverage lists missing outcomes and ignores case", {
  rr <- make_gbd_rr_matrix()
  rr <- rr[rr$disease != "Asthma", ]
  rr$disease <- toupper(rr$disease)

  gaps <- check_gbd_rr_coverage(rr, obesity_cost_outcome_mapping())

  expect_equal(gaps$outcome, "Asthma")
  expect_equal(gaps$disease_group, "asthma")
})

test_that("check_gbd_rr_coverage validates required columns", {
  expect_error(
    check_gbd_rr_coverage(data.frame(x = 1), obesity_cost_outcome_mapping()),
    "rr_matrix is missing required column"
  )
  expect_error(
    check_gbd_rr_coverage(make_gbd_rr_matrix(), data.frame(outcome_pattern = "a")),
    "outcome_mapping is missing required column\\(s\\): disease_group"
  )
})
