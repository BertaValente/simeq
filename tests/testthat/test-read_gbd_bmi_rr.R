write_gbd_workbook <- function(cells) {
  skip_if_not_installed("writexl")
  age_cols <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
                "85-89", "90-94", "95+")
  wb <- data.frame(disease = "Asthma", sex = "Males", check.names = FALSE)
  for (a in age_cols) wb[[a]] <- ""
  wb[names(cells)] <- cells
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(wb, path)
  path
}

test_that("read_gbd_bmi_rr parses point estimates and intervals", {
  path <- write_gbd_workbook(list(
    "20-24" = "1.32 (1.20 to 1.45)",
    "25-29" = "1.10 (1.05–1.16)"
  ))

  rr <- read_gbd_bmi_rr(path)

  expect_equal(nrow(rr), 2L)
  expect_equal(rr$age_band, c("20-24", "25-29"))
  expect_equal(rr$rr_per_5, c(1.32, 1.10))
  expect_equal(rr$rr_low, c(1.20, 1.05))
  expect_equal(rr$rr_high, c(1.45, 1.16))
})

test_that("read_gbd_bmi_rr errors on a missing file", {
  expect_error(read_gbd_bmi_rr(tempfile(fileext = ".xlsx")), "was not found")
})

test_that("read_gbd_bmi_rr errors when no RR cells are filled", {
  expect_error(read_gbd_bmi_rr(write_gbd_workbook(list())), "No relative-risk records")
})

test_that("read_gbd_bmi_rr rejects unparseable point estimates", {
  path <- write_gbd_workbook(list("20-24" = "n/a"))

  expect_error(read_gbd_bmi_rr(path), "invalid relative-risk point estimates")
})
