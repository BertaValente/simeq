#' Use a bound of the GBD relative-risk interval as the relative risk
#'
#' Replaces `rr_per_5` with the lower or upper bound of its 95% uncertainty
#' interval, for one-way sensitivity analyses of the relative risks. Every
#' outcome, sex and age band moves to the same bound together, so the result
#' brackets the effect of relative-risk uncertainty rather than giving a
#' probabilistic interval.
#'
#' Rows whose bound is missing keep their point estimate.
#'
#' @param rr_matrix A data frame returned by [read_gbd_bmi_rr()].
#' @param bound `"point"` (unchanged), `"lower"` (`rr_low`) or `"upper"`
#'   (`rr_high`).
#'
#' @return `rr_matrix` with `rr_per_5` replaced by the chosen bound and a
#'   `rr_bound` column naming it.
#' @export
select_gbd_rr_bound <- function(
    rr_matrix,
    bound = c("point", "lower", "upper")
) {
  bound <- match.arg(bound)
  required_columns <- c("rr_per_5", "rr_low", "rr_high")
  missing_columns <- setdiff(required_columns, names(rr_matrix))
  if (length(missing_columns) > 0L) {
    stop(
      "rr_matrix is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  replacement <- switch(
    bound,
    point = rr_matrix$rr_per_5,
    lower = rr_matrix$rr_low,
    upper = rr_matrix$rr_high
  )
  replacement <- as.numeric(replacement)
  use_point <- is.na(replacement)
  replacement[use_point] <- rr_matrix$rr_per_5[use_point]

  if (any(!is.finite(replacement)) || any(replacement <= 0)) {
    stop(
      "The ", bound, " relative-risk bound contains invalid values.",
      call. = FALSE
    )
  }

  rr_matrix$rr_per_5 <- replacement
  rr_matrix$rr_bound <- bound
  rr_matrix
}
