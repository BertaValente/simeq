rr_bound_fixture <- function() {
  data.frame(
    disease = c("Asthma", "Gout"),
    sex = c("Males", "Females"),
    age_band = c("20-24", "25-29"),
    rr_per_5 = c(1.30, 1.50),
    rr_low = c(1.20, NA),
    rr_high = c(1.45, 1.70),
    stringsAsFactors = FALSE
  )
}

test_that("select_gbd_rr_bound swaps in the requested bound", {
  rr <- rr_bound_fixture()

  expect_equal(select_gbd_rr_bound(rr, "point")$rr_per_5, c(1.30, 1.50))
  expect_equal(select_gbd_rr_bound(rr, "upper")$rr_per_5, c(1.45, 1.70))
  expect_equal(select_gbd_rr_bound(rr, "upper")$rr_bound, c("upper", "upper"))
})

test_that("select_gbd_rr_bound keeps the point estimate when a bound is missing", {
  expect_equal(
    select_gbd_rr_bound(rr_bound_fixture(), "lower")$rr_per_5,
    c(1.20, 1.50)
  )
})

test_that("select_gbd_rr_bound validates its input", {
  rr <- rr_bound_fixture()
  expect_error(select_gbd_rr_bound(rr[, c("disease", "rr_per_5")], "lower"),
               "missing required column")
  expect_error(select_gbd_rr_bound(rr, "middle"))
  rr$rr_high[1] <- -1
  expect_error(select_gbd_rr_bound(rr, "upper"), "invalid values")
})
