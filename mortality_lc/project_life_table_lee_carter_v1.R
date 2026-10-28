# Lee-Carter projection of Portuguese life tables, ages 20-84, 2025-2040
#
# Fits log m(x,t) = a(x) + b(x) k(t) separately by sex to the observed
# 1998-2024 tables, leaving out the COVID-19 years 2020-2021. k(t) is
# projected as a random walk with drift. Projected rates start from the
# observed 2024 rates (jump-off at the last observed year), so 2025 continues
# smoothly from the table the model already uses:
#   log m(x,t) = log m(x,2024) + b(x) [k(t) - k(2024)]
#
# Output: the 1998-2024 model input plus projected years 2025-2040, with a
# `projected` flag. Base R only.

project_root <- "."   # set to the project folder
lt_file <- file.path(project_root, "data", "life_tables",
                     "pt_life_table_model_input_1998_2024.csv")
out_file <- file.path(project_root, "data", "life_tables",
                      "pt_life_table_model_input_1998_2040_lc.csv")

fit_years     <- setdiff(1998:2024, c(2020L, 2021L))
jump_off_year <- 2024L
proj_years    <- 2025:2040

lt <- utils::read.csv(lt_file)
lt$mx <- -log(1 - lt$qx)   # constant force of mortality within each age

fit_lc <- function(d) {
  ages  <- sort(unique(d$age))
  years <- sort(unique(d$year))
  M <- matrix(log(d$mx[order(d$year, d$age)]), nrow = length(ages),
              dimnames = list(ages, years))
  fit_cols <- as.character(intersect(fit_years, years))
  a <- rowMeans(M[, fit_cols])
  s <- svd(M[, fit_cols] - a)
  b <- s$u[, 1] / sum(s$u[, 1])
  k <- s$d[1] * s$v[, 1] * sum(s$u[, 1])
  names(k) <- fit_cols
  # Drift of the random walk, from first to last fitted year
  n <- length(k)
  drift <- (k[n] - k[1]) / (as.integer(names(k)[n]) - as.integer(names(k)[1]))
  # k at the jump-off year (fitted from the observed 2024 rates)
  k_jo <- sum(b * (M[, as.character(jump_off_year)] - a)) / sum(b^2)
  log_m_jo <- M[, as.character(jump_off_year)]
  proj <- do.call(rbind, lapply(proj_years, function(t) {
    log_m <- log_m_jo + b * drift * (t - jump_off_year)
    data.frame(year = t, age = ages, mx = exp(log_m))
  }))
  list(proj = proj,
       params = data.frame(age = ages, a = a, b = b),
       k = data.frame(year = as.integer(names(k)), k = k),
       drift = drift,
       var_explained = s$d[1]^2 / sum(s$d^2))
}

fits <- lapply(split(lt, lt$sex), fit_lc)

projected <- do.call(rbind, lapply(names(fits), function(s) {
  p <- fits[[s]]$proj
  p$sex <- s
  p
}))
projected$qx <- round(1 - exp(-projected$mx), 6)
projected$ex <- NA_real_   # not needed: YLL uses the 2019 table
projected$projected <- TRUE

observed <- lt[, c("year", "age", "sex", "qx", "ex")]
observed$projected <- FALSE

out <- rbind(observed, projected[, names(observed)])
out <- out[order(out$year, out$sex, out$age), ]
utils::write.csv(out, out_file, row.names = FALSE)

# Summary
for (s in names(fits)) {
  cat(sprintf("%s: variance explained %.1f%%, drift in k %.3f per year\n",
              s, 100 * fits[[s]]$var_explained, fits[[s]]$drift))
}
