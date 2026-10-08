#' Create PONTE-O simulation settings
#'
#' Creates a validated settings object for a PONTE-O simulation.
#'
#' @param initial_year Integer. First year represented in the baseline
#'   population.
#' @param projection_end_year Integer. Final simulation year.
#' @param entry_age Integer. Age at which new population entrants join
#'   the simulation.
#' @param max_age Integer. Maximum represented age.
#' @param n_simulations Integer. Number of stochastic simulation
#'   replications.
#' @param entrant_n_per_year Integer. Number of sampled entrants per
#'   simulation year.
#' @param seed Integer. Random-number seed.
#'
#' @return An object of class `"simeq_settings"`.
#' @export
#'
#' @examples
#' settings <- simeq_settings(
#'   initial_year = 2020,
#'   projection_end_year = 2040,
#'   n_simulations = 100
#' )
simeq_settings <- function(
    initial_year = 2020L,
    projection_end_year = 2040L,
    entry_age = 20L,
    max_age = 84L,
    n_simulations = 100L,
    entrant_n_per_year = 1000L,
    seed = 1234L
) {
  integer_arguments <- list(
    initial_year = initial_year,
    projection_end_year = projection_end_year,
    entry_age = entry_age,
    max_age = max_age,
    n_simulations = n_simulations,
    entrant_n_per_year = entrant_n_per_year,
    seed = seed
  )

  invalid_integer_arguments <- names(integer_arguments)[
    !vapply(
      integer_arguments,
      function(x) {
        length(x) == 1L &&
          !is.na(x) &&
          is.finite(x) &&
          x == as.integer(x)
      },
      logical(1)
    )
  ]

  if (length(invalid_integer_arguments) > 0L) {
    stop(
      "The following arguments must be single finite integers: ",
      paste(invalid_integer_arguments, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  initial_year <- as.integer(initial_year)
  projection_end_year <- as.integer(projection_end_year)
  entry_age <- as.integer(entry_age)
  max_age <- as.integer(max_age)
  n_simulations <- as.integer(n_simulations)
  entrant_n_per_year <- as.integer(entrant_n_per_year)
  seed <- as.integer(seed)

  if (projection_end_year < initial_year) {
    stop(
      "`projection_end_year` must be greater than or equal to ",
      "`initial_year`.",
      call. = FALSE
    )
  }

  if (entry_age < 0L) {
    stop(
      "`entry_age` must be zero or greater.",
      call. = FALSE
    )
  }

  if (max_age <= entry_age) {
    stop(
      "`max_age` must be greater than `entry_age`.",
      call. = FALSE
    )
  }

  if (n_simulations < 1L) {
    stop(
      "`n_simulations` must be at least 1.",
      call. = FALSE
    )
  }

  if (entrant_n_per_year < 2L) {
    stop(
      "`entrant_n_per_year` must be at least 2.",
      call. = FALSE
    )
  }

  structure(
    list(
      initial_year = initial_year,
      projection_end_year = projection_end_year,
      entry_age = entry_age,
      max_age = max_age,
      n_simulations = n_simulations,
      entrant_n_per_year = entrant_n_per_year,
      seed = seed
    ),
    class = "simeq_settings"
  )
}

#' @export
print.simeq_settings <- function(
    x,
    ...
) {
  cat("PONTE-O simulation settings\n")
  cat(
    "  Projection years: ",
    x$initial_year,
    " to ",
    x$projection_end_year,
    "\n",
    sep = ""
  )
  cat(
    "  Age range: ",
    x$entry_age,
    " to ",
    x$max_age,
    "\n",
    sep = ""
  )
  cat(
    "  Simulations: ",
    x$n_simulations,
    "\n",
    sep = ""
  )
  cat(
    "  Entrants per year: ",
    x$entrant_n_per_year,
    "\n",
    sep = ""
  )
  cat(
    "  Seed: ",
    x$seed,
    "\n",
    sep = ""
  )

  invisible(x)
}
