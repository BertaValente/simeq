#' Prepare a year-indexed life table from Human Mortality Database tables
#'
#' Converts one or more period life tables in Human Mortality Database (HMD)
#' layout (columns `Year`, `Age`, `qx`, `ex`, as in `mltper_1x1.txt` and
#' `fltper_1x1.txt`) into the long format used by [simeq_run_simulation()]
#' (`year`, `age`, `sex`, `qx`) and by [simeq_calculate_yll()]
#' (`ref_life_expectancy`: `age`, `sex`, `ex`). Column names are matched
#' case-insensitively and an open age group such as `"110+"` is read as 110.
#'
#' @param tables A named list of data frames or file paths, one per sex, with
#'   names `"men"` and/or `"women"`. A path is read as an HMD text file
#'   (two header lines, then whitespace-separated columns) or, if it ends in
#'   `.csv`, as a comma-separated file.
#' @param min_age,max_age Age range to keep.
#' @param years Optional integer vector of calendar years to keep.
#'
#' The result is validated with [simeq_validate_life_table()] when both sexes
#' are supplied.
#'
#' @return A data frame with columns `year`, `age`, `sex`, `qx`, and `ex`.
#'   Pass it directly as `life_table` (the extra `ex` column is ignored), and
#'   filter one year and keep `age`, `sex`, `ex` for `ref_life_expectancy`.
#' @export
#'
#' @examples
#' men <- data.frame(Year = 2020, Age = 20:84, qx = 0.01, ex = 60:(-4))
#' simeq_life_table_from_hmd(list(men = men))
simeq_life_table_from_hmd <- function(
    tables,
    min_age = 20L,
    max_age = 84L,
    years = NULL
) {
  if (!is.list(tables) || is.data.frame(tables) || is.null(names(tables)) ||
      !all(names(tables) %in% c("men", "women")) ||
      anyDuplicated(names(tables))) {
    stop(
      "`tables` must be a list named with \"men\" and/or \"women\".",
      call. = FALSE
    )
  }

  out <- lapply(names(tables), function(sex) {
    x <- tables[[sex]]
    if (is.character(x)) {
      x <- .read_hmd_table(x)
    }
    if (!is.data.frame(x)) {
      stop("Each element of `tables` must be a data frame or a file path.",
           call. = FALSE)
    }
    names(x) <- tolower(names(x))
    missing_columns <- setdiff(c("year", "age", "qx"), names(x))
    if (length(missing_columns) > 0L) {
      stop(
        "Life table for ", sex, " is missing columns: ",
        paste(missing_columns, collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (!"ex" %in% names(x)) {
      x$ex <- NA_real_
    }
    data.frame(
      year = as.integer(x$year),
      age = as.integer(sub("\\+$", "", trimws(as.character(x$age)))),
      sex = sex,
      qx = suppressWarnings(as.numeric(x$qx)),
      ex = suppressWarnings(as.numeric(x$ex)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)

  out <- out[out$age >= min_age & out$age <= max_age, , drop = FALSE]
  if (!is.null(years)) {
    out <- out[out$year %in% years, , drop = FALSE]
  }
  out <- out[order(out$year, out$sex, out$age), , drop = FALSE]
  rownames(out) <- NULL

  if (nrow(out) == 0L) {
    stop("No life-table rows remain after filtering.", call. = FALSE)
  }

  # The model needs both sexes; validate once both are supplied.
  if (all(c("men", "women") %in% out$sex)) {
    simeq_validate_life_table(
      out[, c("year", "age", "sex", "qx")],
      entry_age = min_age,
      max_age = max_age
    )
  }

  out
}

.read_hmd_table <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }
  lines <- readLines(path, warn = FALSE)
  header_line <- grep("^\\s*Year\\s+Age", lines, ignore.case = TRUE)[1L]
  if (is.na(header_line)) {
    stop("Could not find a `Year Age ...` header in ", path, call. = FALSE)
  }
  utils::read.table(
    text = lines[header_line:length(lines)],
    header = TRUE,
    fill = TRUE,
    stringsAsFactors = FALSE,
    colClasses = c(Age = "character")
  )
}
