make_annual_cost_fixtures <- function() {
  age_bands <- paste0(seq(20, 80, 5), "-", seq(24, 84, 5))

  rr_matrix <- expand.grid(
    disease = "Example disease",
    sex = c("Males", "Females"),
    age_band = age_bands,
    stringsAsFactors = FALSE
  )
  rr_matrix$rr_per_5 <- 1.5
  rr_matrix$rr_low <- 1.4
  rr_matrix$rr_high <- 1.6

  make_people <- function(simulation, obese_share) {
    people <- expand.grid(
      age = 20:84,
      sex = c("men", "women"),
      stringsAsFactors = FALSE
    )
    people$simulation <- simulation
    people$year <- 2020L
    people$alive <- TRUE
    people$survey_weight <- ifelse(people$sex == "men", 100, 200)
    people$bmi <- ifelse(
      seq_len(nrow(people)) %% 10L < obese_share * 10L,
      32,
      23
    )
    people
  }

  simulation_output <- rbind(
    make_people(1L, 0.2),
    make_people(2L, 0.4)
  )

  costs_dro <- tibble::tibble(
    disease_group   = c("example_group", "obesity_overweight"),
    inpatient       = c(100, 10),
    outpatient      = c(200, 20),
    pharmacological = c(300, 30),
    total           = c(600, 60)
  )

  list(
    simulation_output = simulation_output,
    rr_matrix = rr_matrix,
    costs_dro = costs_dro,
    outcome_mapping = tibble::tibble(
      outcome_pattern = "Example disease",
      disease_group = "example_group"
    )
  )
}

run_annual_costs <- function(f, ...) {
  calculate_annual_obesity_costs(
    simulation_output = f$simulation_output,
    gbd_rr_matrix = f$rr_matrix,
    costs_dro = f$costs_dro,
    outcome_mapping = f$outcome_mapping,
    exclude_disease_groups = "obesity_overweight",
    years = 2020L,
    base_cost_year = 2020L,
    ...
  )
}

test_that("annual costs add direct treatment costs", {
  f <- make_annual_cost_fixtures()

  with_direct <- run_annual_costs(f)
  without_direct <- run_annual_costs(f, include_direct_costs = FALSE)

  expect_equal(
    with_direct$totals$total_keur_2018_constant -
      without_direct$totals$total_keur_2018_constant,
    60
  )
  expect_true("obesity_overweight" %in% with_direct$by_year_disease$disease_group)
})

test_that("annual costs by sex add up to the national total", {
  f <- make_annual_cost_fixtures()

  result <- run_annual_costs(f)

  expect_equal(
    sum(result$by_sex_totals$total_keur_2018_constant),
    result$totals$total_keur_2018_constant
  )
})

test_that("annual costs give per-replication totals and intervals", {
  f <- make_annual_cost_fixtures()
  # Replications differ in later years; both reproduce the base year.
  later <- f$simulation_output
  later$year <- 2021L
  later$bmi[later$simulation == 2L & later$bmi < 30] <- 31
  f$simulation_output <- rbind(f$simulation_output, later)

  result <- calculate_annual_obesity_costs(
    simulation_output = f$simulation_output,
    gbd_rr_matrix = f$rr_matrix,
    costs_dro = f$costs_dro,
    outcome_mapping = f$outcome_mapping,
    exclude_disease_groups = "obesity_overweight",
    years = 2021L,
    base_cost_year = 2020L,
    by_simulation = TRUE
  )

  expect_equal(nrow(result$totals_by_simulation), 2L)
  expect_true(all(c(
    "total_keur_2018_constant_lo",
    "total_keur_2018_constant_hi"
  ) %in% names(result$totals)))
  expect_lt(
    result$totals$total_keur_2018_constant_lo,
    result$totals$total_keur_2018_constant_hi
  )
})

test_that("direct cost group must be excluded from PAF rescaling", {
  f <- make_annual_cost_fixtures()

  expect_error(
    run_annual_costs(f, exclude_disease_groups = character()),
    "exclude_disease_groups"
  )
})

test_that("base year without living people is an error", {
  f <- make_annual_cost_fixtures()

  expect_error(
    run_annual_costs(f, base_cost_year = 2018L),
    "base_cost_year"
  )
})

make_two_year_output <- function() {
  f <- make_annual_cost_fixtures()
  base <- f$simulation_output[f$simulation_output$simulation == 1L, ]
  later <- f$simulation_output[f$simulation_output$simulation == 2L, ]
  base$year <- 2018L
  later$year <- 2020L
  later$simulation <- 1L
  # Give some non-obese people overweight BMI in both years.
  base$bmi[base$bmi == 23][c(TRUE, FALSE)] <- 27
  later$bmi[later$bmi == 23][c(TRUE, FALSE)] <- 27
  f$simulation_output <- rbind(base, later)
  f
}

run_two_years <- function(f, ...) {
  calculate_annual_obesity_costs(
    simulation_output = f$simulation_output,
    gbd_rr_matrix = f$rr_matrix,
    costs_dro = f$costs_dro,
    outcome_mapping = f$outcome_mapping,
    exclude_disease_groups = "obesity_overweight",
    years = c(2018L, 2020L),
    base_cost_year = 2018L,
    ...
  )
}

test_that("the base year reproduces the published costs", {
  f <- make_two_year_output()

  by_year <- run_two_years(f, include_direct_costs = FALSE)$by_year_disease

  expect_equal(by_year$total_obesity[by_year$year == 2018L], 600)
  expect_gt(by_year$total_obesity[by_year$year == 2020L], 600)
})

test_that("obesity-only costs are a share of the high-BMI costs", {
  f <- make_two_year_output()

  high_bmi <- run_two_years(f, include_direct_costs = FALSE)$by_year_disease
  obesity <- run_two_years(
    f,
    include_direct_costs = FALSE,
    exposure = "obesity"
  )$by_year_disease

  expect_lt(
    obesity$total_obesity[obesity$year == 2018L],
    high_bmi$total_obesity[high_bmi$year == 2018L]
  )
  expect_gt(obesity$total_obesity[obesity$year == 2018L], 0)
})

test_that("direct treatment costs follow exposure prevalence", {
  f <- make_two_year_output()

  res <- run_two_years(f, exposure = "obesity")
  direct <- res$by_year_disease[
    res$by_year_disease$disease_group == "obesity_overweight",
  ]

  expect_equal(direct$total_obesity[direct$year == 2018L], 60)
  prev <- function(yr) {
    d <- f$simulation_output[f$simulation_output$year == yr, ]
    sum(d$survey_weight[d$bmi >= 30]) / sum(d$survey_weight)
  }
  expect_equal(
    direct$total_obesity[direct$year == 2020L],
    60 * prev(2020L) / prev(2018L)
  )
})

test_that("heavier obesity costs more", {
  f <- make_two_year_output()
  heavy <- f
  heavy$simulation_output$bmi[heavy$simulation_output$year == 2020L &
                                heavy$simulation_output$bmi >= 30] <- 40

  mild <- run_two_years(f, include_direct_costs = FALSE)$by_year_disease
  severe <- run_two_years(heavy, include_direct_costs = FALSE)$by_year_disease

  expect_gt(
    severe$total_obesity[severe$year == 2020L],
    mild$total_obesity[mild$year == 2020L]
  )
})

test_that("calculate_obesity_pafs_individual follows the PAF formula", {
  people <- data.frame(
    year = 2020L,
    age = rep(20:84, each = 2),
    sex = rep(c("men", "women"), 65),
    alive = TRUE,
    survey_weight = 1,
    bmi = 22,
    stringsAsFactors = FALSE
  )
  people <- rbind(people, transform(people, bmi = 32))
  rr <- expand.grid(
    disease = "d",
    sex = c("Males", "Females"),
    age_band = paste0(seq(20, 80, 5), "-", seq(24, 84, 5)),
    stringsAsFactors = FALSE
  )
  rr$rr_per_5 <- 1.5

  paf <- calculate_obesity_pafs_individual(people, 2020L, rr)$paf_by_outcome

  # Within each sex, half at the reference and half at RR 1.5^2.
  mean_rr <- (1 + 1.5^2) / 2
  expect_equal(paf$paf_obesity, (mean_rr - 1) / mean_rr)
})
