#' Age a living synthetic population by one year
#'
#' Ages people by one year. People who would pass `max_age` leave the
#' modelled population (they are not recorded as deaths); mortality for their
#' last modelled year has already been applied at `max_age`. The calendar
#' year is updated separately by [simeq_advance_year()].
#'
#' @param state A living PONTE-PT population state.
#' @param entry_age Integer minimum represented age.
#' @param max_age Integer maximum represented age.
#'
#' @return A living population state with ages advanced by one year, without
#'   the people who passed `max_age`.
#' @export
simeq_age_population <- function(
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

  state$age <- as.integer(state$age) + 1L

  state[state$age <= as.integer(max_age), , drop = FALSE]
}
