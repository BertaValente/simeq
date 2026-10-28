#' Validate annual entrant population targets
#'
#' Validates annual sex-specific population targets used to weight age-20
#' entrants in the open SIMEQ cohort.
#'
#' @param entrant_targets A data frame with `entry_year`, `sex`, and
#'   `entrant_population`.
#' @param start_year Integer. First year requiring entrant targets.
#' @param end_year Integer. Last year requiring entrant targets.
#'
#' @return The input `entrant_targets`, invisibly, if all checks pass.
#' @export
#'
#' @examples
#' entrant_targets <- expand.grid(
#'   entry_year = 2021:2023,
#'   sex = c("men", "women"),
#'   stringsAsFactors = FALSE
#' )
#'
#' entrant_targets$entrant_population <- 50000
#'
#' simeq_validate_entrant_targets(
#'   entrant_targets = entrant_targets,
#'   start_year = 2021L,
#'   end_year = 2023L
#' )
simeq_validate_entrant_targets <- function(
    entrant_targets,
    start_year,
    end_year
) {
  if (!is.data.frame(entrant_targets)) {
    stop(
      "`entrant_targets` must be a data frame.",
      call. = FALSE
    )
  }

  if (nrow(entrant_targets) == 0L) {
    stop(
      "`entrant_targets` must contain at least one record.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(start_year) ||
    length(start_year) != 1L ||
    is.na(start_year) ||
    !is.finite(start_year) ||
    start_year != as.integer(start_year)
  ) {
    stop(
      "`start_year` must be one finite integer value.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(end_year) ||
    length(end_year) != 1L ||
    is.na(end_year) ||
    !is.finite(end_year) ||
    end_year != as.integer(end_year)
  ) {
    stop(
      "`end_year` must be one finite integer value.",
      call. = FALSE
    )
  }

  start_year <- as.integer(
    start_year
  )

  end_year <- as.integer(
    end_year
  )

  if (end_year < start_year) {
    stop(
      "`end_year` must not be earlier than `start_year`.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "entry_year",
    "sex",
    "entrant_population"
  )

  missing_columns <- setdiff(
    required_columns,
    names(entrant_targets)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`entrant_targets` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  entry_year <- entrant_targets$entry_year

  if (
    !is.numeric(entry_year) ||
    anyNA(entry_year) ||
    any(!is.finite(entry_year)) ||
    any(entry_year != as.integer(entry_year))
  ) {
    stop(
      "`entry_year` must contain finite, non-missing integer values.",
      call. = FALSE
    )
  }

  if (any(entry_year < start_year | entry_year > end_year)) {
    stop(
      "`entrant_targets` must cover only years from ",
      start_year,
      " to ",
      end_year,
      ".",
      call. = FALSE
    )
  }

  sex <- as.character(
    entrant_targets$sex
  )

  required_sexes <- c(
    "men",
    "women"
  )

  if (
    anyNA(sex) ||
    any(!sex %in% required_sexes)
  ) {
    stop(
      "`sex` must contain only `men` and `women`.",
      call. = FALSE
    )
  }

  entrant_population <- entrant_targets$entrant_population

  if (
    !is.numeric(entrant_population) ||
    anyNA(entrant_population) ||
    any(!is.finite(entrant_population)) ||
    any(entrant_population <= 0)
  ) {
    stop(
      "`entrant_population` must contain positive, finite values.",
      call. = FALSE
    )
  }

  target_identifier <- paste(
    as.integer(entry_year),
    sex,
    sep = "::"
  )

  if (anyDuplicated(target_identifier) > 0L) {
    stop(
      "`entrant_targets` must contain one record per year-sex combination.",
      call. = FALSE
    )
  }

  expected_targets <- expand.grid(
    entry_year = seq.int(
      from = start_year,
      to = end_year
    ),
    sex = required_sexes,
    stringsAsFactors = FALSE
  )

  expected_identifier <- paste(
    expected_targets$entry_year,
    expected_targets$sex,
    sep = "::"
  )

  if (any(!expected_identifier %in% target_identifier)) {
    stop(
      "`entrant_targets` must cover every year-sex combination from ",
      start_year,
      " to ",
      end_year,
      ".",
      call. = FALSE
    )
  }

  invisible(entrant_targets)
}
