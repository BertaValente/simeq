#' Age a living synthetic population by one year
#'
#' Ages people by one year, capping at the maximum modelled age rather than
#' removing them. People who reach `max_age` remain in the state at that age
#' and continue to be exposed to mortality in subsequent cycles; they exit only
#' when they die. The calendar year is updated separately by
#' [ponte_advance_year()].
#'
#' @param state A living PONTE-PT population state.
#' @param entry_age Integer minimum represented age.
#' @param max_age Integer maximum represented age.
#'
#' @return A living population state with ages advanced by one year (capped at
#'   `max_age`). Row count is unchanged.
#' @export
ponte_age_population <- function(
    state,
    entry_age = 20L,
    max_age = 84L
) {
  if (!is.data.frame(state)) {
    stop("`state` must be a data frame.", call. = FALSE)
  }

  if (!"age" %in% names(state)) {
    stop("`state` must contain an `age` column.", call. = FALSE)
  }

  if ("alive" %in% names(state) && any(!state$alive, na.rm = TRUE)) {
    stop(
      "`state` must contain only living records.",
      call. = FALSE
    )
  }

  state$age <- pmin(
    as.integer(state$age) + 1L,
    as.integer(max_age)
  )

  state
}
