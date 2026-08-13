#' Validate a PONTE-PT-O life table
#'
#' Validates a long-format life table containing annual probabilities of death
#' by exact age and sex. The `qx` value is the probability that a person aged
#' exactly `age` dies before reaching the next age.
#'
#' @param life_table A data frame with columns `age`, `sex`, and `qx`.
#' @param entry_age Integer. Minimum represented age.
#' @param max_age Integer. Maximum represented age.
#'
#' @return The input `life_table`, invisibly, if all checks pass.
#' @export
#'
#' @examples
#' life_table <- data.frame(
#'   age = rep(20:84, each = 2),
#'   sex = rep(c("men", "women"), times = 65),
#'   qx = 0.01
#' )
#'
#' ponte_validate_life_table(life_table)
ponte_validate_life_table <- function(
    life_table,
    entry_age = 20L,
    max_age = 84L
) {
  if (!is.data.frame(life_table)) {
    stop(
      "`life_table` must be a data frame.",
      call. = FALSE
    )
  }

  if (nrow(life_table) == 0L) {
    stop(
      "`life_table` must contain at least one record.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "age",
    "sex",
    "qx"
  )

  missing_columns <- setdiff(
    required_columns,
    names(life_table)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`life_table` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  age <- life_table$age

  if (
    !is.numeric(age) ||
    anyNA(age) ||
    any(!is.finite(age)) ||
    any(age != as.integer(age))
  ) {
    stop(
      "`age` must contain finite, non-missing integer values.",
      call. = FALSE
    )
  }

  if (any(age < entry_age | age > max_age)) {
    stop(
      "`life_table` must cover only ages from ",
      entry_age,
      " to ",
      max_age,
      ".",
      call. = FALSE
    )
  }

  sex <- as.character(
    life_table$sex
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

  qx <- life_table$qx

  if (
    !is.numeric(qx) ||
    anyNA(qx) ||
    any(!is.finite(qx)) ||
    any(qx < 0 | qx > 1)
  ) {
    stop(
      "`qx` must contain finite probabilities from 0 to 1.",
      call. = FALSE
    )
  }

  life_table_identifier <- paste(
    as.integer(age),
    sex,
    sep = "::"
  )

  if (anyDuplicated(life_table_identifier) > 0L) {
    stop(
      "`life_table` must contain one record per age-sex combination.",
      call. = FALSE
    )
  }

  expected_cells <- expand.grid(
    age = seq.int(
      from = as.integer(entry_age),
      to = as.integer(max_age)
    ),
    sex = required_sexes,
    stringsAsFactors = FALSE
  )

  observed_cells <- data.frame(
    age = as.integer(age),
    sex = sex,
    stringsAsFactors = FALSE
  )

  expected_identifier <- paste(
    expected_cells$age,
    expected_cells$sex,
    sep = "::"
  )

  observed_identifier <- paste(
    observed_cells$age,
    observed_cells$sex,
    sep = "::"
  )

  if (any(!expected_identifier %in% observed_identifier)) {
    stop(
      "`life_table` must cover every age-sex combination from ",
      entry_age,
      " to ",
      max_age,
      ".",
      call. = FALSE
    )
  }

  invisible(life_table)
}
