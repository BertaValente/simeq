#' Format QALY trajectory results
#'
#' Formats annual QALYs per person and their 95% simulation intervals into a
#' report-ready table stratified by obesity status.
#'
#' @param qaly_results A list returned by [ponte_calculate_qalys()].
#'
#' @return A data frame containing year, sex, education level, obesity status,
#'   and formatted QALYs per person with 95% simulation intervals.
#'
#' @export
ponte_format_qaly_table <- function(
    qaly_results
) {
  if (
    !is.list(qaly_results) ||
    is.null(qaly_results$by_year_obesity)
  ) {
    stop(
      "`qaly_results` must be a list containing `by_year_obesity`.",
      call. = FALSE
    )
  }

  by_year_obesity <- qaly_results$by_year_obesity

  required_columns <- c(
    "year",
    "sex",
    "education_level",
    "obesity_status",
    "mean_qaly_per_person",
    "qaly_lo",
    "qaly_hi"
  )

  missing_columns <- setdiff(
    required_columns,
    names(by_year_obesity)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`qaly_results$by_year_obesity` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  format_qaly <- function(x) {
    formatC(
      x,
      format = "f",
      digits = 3
    )
  }

  data.frame(
    year = by_year_obesity$year,
    sex = by_year_obesity$sex,
    education_level = by_year_obesity$education_level,
    obesity_status = by_year_obesity$obesity_status,
    qaly_per_person_95ci = paste0(
      format_qaly(by_year_obesity$mean_qaly_per_person),
      " (",
      format_qaly(by_year_obesity$qaly_lo),
      "\u2013",
      format_qaly(by_year_obesity$qaly_hi),
      ")"
    ),
    stringsAsFactors = FALSE
  )
}
