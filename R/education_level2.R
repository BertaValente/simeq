#' Create or validate two-level education classification
#'
#' Returns a population state with an `education_level2` variable suitable for
#' the SIMEQ mortality model. The two supported categories are `lower`
#' and `higher`.
#'
#' @param state A data frame containing population or simulation-state data.
#' @param source Character string. Either `"existing"` to validate an existing
#'   `education_level2` column, or `"map_3_to_2"` to derive it from a
#'   three-level `education_level` column.
#'
#' @return A data frame containing a valid `education_level2` column.
#' @export
#'
#' @examples
#' state <- data.frame(
#'   education_level = c("low", "medium", "high")
#' )
#'
#' simeq_prepare_education_level2(
#'   state = state,
#'   source = "map_3_to_2"
#' )
simeq_prepare_education_level2 <- function(
    state,
    source = c(
      "existing",
      "map_3_to_2"
    )
) {
  if (!is.data.frame(state)) {
    stop(
      "`state` must be a data frame.",
      call. = FALSE
    )
  }

  source <- match.arg(source)

  if (identical(source, "existing")) {
    if (!"education_level2" %in% names(state)) {
      stop(
        "`state` must contain `education_level2` when ",
        "`source = \"existing\"`.",
        call. = FALSE
      )
    }

    education_level2 <- as.character(
      state$education_level2
    )

    if (
      anyNA(education_level2) ||
      any(
        !education_level2 %in% c(
          "lower",
          "higher"
        )
      )
    ) {
      stop(
        "`education_level2` must contain only `lower` and `higher`.",
        call. = FALSE
      )
    }

    state$education_level2 <- education_level2

    return(state)
  }

  if (!"education_level" %in% names(state)) {
    stop(
      "`state` must contain `education_level` when ",
      "`source = \"map_3_to_2\"`.",
      call. = FALSE
    )
  }

  education_level <- as.character(
    state$education_level
  )

  expected_levels <- c(
    "low",
    "medium",
    "high"
  )

  if (
    anyNA(education_level) ||
    any(!education_level %in% expected_levels)
  ) {
    stop(
      "`education_level` must contain only `low`, `medium`, and `high` ",
      "when `source = \"map_3_to_2\"`.",
      call. = FALSE
    )
  }

  state$education_level2 <- ifelse(
    education_level %in% c(
      "low",
      "medium"
    ),
    "lower",
    "higher"
  )

  state
}
