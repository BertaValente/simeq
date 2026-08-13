#' Format obesity-attributable burden results
#'
#' Formats annual obesity-attributable YLL, YLD, and DALY rates and their
#' 95% simulation intervals into a report-ready table.
#'
#' @param burden_results A list returned by
#'   [ponte_calculate_obesity_burden()].
#'
#' @return A data frame containing year, sex, education level, and formatted
#'   YLL, YLD, and DALY rates per 100,000 with 95% simulation intervals.
#'
#' @export
ponte_format_obesity_burden_table <- function(
    burden_results
) {
  if (
    !is.list(burden_results) ||
    is.null(burden_results$by_year)
  ) {
    stop(
      "`burden_results` must be a list containing `by_year`.",
      call. = FALSE
    )
  }

  by_year <- burden_results$by_year

  required_columns <- c(
    "year",
    "sex",
    "education_level",
    "yll_obesity_per100k",
    "yll_obesity_per100k_lo",
    "yll_obesity_per100k_hi",
    "yld_obesity_per100k",
    "yld_obesity_per100k_lo",
    "yld_obesity_per100k_hi",
    "daly_obesity_per100k",
    "daly_obesity_per100k_lo",
    "daly_obesity_per100k_hi"
  )

  missing_columns <- setdiff(
    required_columns,
    names(by_year)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`burden_results$by_year` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  format_rate <- function(x) {
    formatC(
      x,
      format = "f",
      digits = 1,
      big.mark = ","
    )
  }

  format_interval <- function(
    estimate,
    lower,
    upper
  ) {
    paste0(
      format_rate(estimate),
      " (",
      format_rate(lower),
      "\u2013",
      format_rate(upper),
      ")"
    )
  }

  data.frame(
    year = by_year$year,
    sex = by_year$sex,
    education_level = by_year$education_level,
    yll_obesity_per100k_95ci = format_interval(
      estimate = by_year$yll_obesity_per100k,
      lower = by_year$yll_obesity_per100k_lo,
      upper = by_year$yll_obesity_per100k_hi
    ),
    yld_obesity_per100k_95ci = format_interval(
      estimate = by_year$yld_obesity_per100k,
      lower = by_year$yld_obesity_per100k_lo,
      upper = by_year$yld_obesity_per100k_hi
    ),
    daly_obesity_per100k_95ci = format_interval(
      estimate = by_year$daly_obesity_per100k,
      lower = by_year$daly_obesity_per100k_lo,
      upper = by_year$daly_obesity_per100k_hi
    ),
    stringsAsFactors = FALSE
  )
}
