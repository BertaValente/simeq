utils::globalVariables(
  c(
    "year",
    "daly_obesity_per100k",
    "daly_obesity_per100k_lo",
    "daly_obesity_per100k_hi"
  )
)

#' Plot obesity-attributable DALY burden
#'
#' Creates annual obesity-attributable DALY rates per 100,000 population with
#' 95% simulation interval ribbons. Panels are stratified by sex and education
#' level.
#'
#' @param burden_results A list returned by
#'   [simeq_calculate_obesity_burden()].
#'
#' @return A ggplot object.
#'
#' @export
simeq_plot_obesity_burden <- function(
    burden_results
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package `ggplot2` is required for plotting. ",
      "Install it with `install.packages(\"ggplot2\")`.",
      call. = FALSE
    )
  }

  if (
    !is.list(burden_results) ||
    is.null(burden_results$by_year)
  ) {
    stop(
      "`burden_results` must be a list containing `by_year`.",
      call. = FALSE
    )
  }

  burden_data <- burden_results$by_year

  required_columns <- c(
    "year",
    "sex",
    "education_level",
    "daly_obesity_per100k",
    "daly_obesity_per100k_lo",
    "daly_obesity_per100k_hi"
  )

  missing_columns <- setdiff(
    required_columns,
    names(burden_data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`burden_results$by_year` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  ggplot2::ggplot(
    burden_data,
    ggplot2::aes(
      x = year,
      y = daly_obesity_per100k
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = daly_obesity_per100k_lo,
        ymax = daly_obesity_per100k_hi
      ),
      fill = "#D55E00",
      alpha = 0.20
    ) +
    ggplot2::geom_line(
      colour = "#D55E00",
      linewidth = 0.8
    ) +
    ggplot2::facet_grid(
      sex ~ education_level
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Obesity-attributable DALYs per 100,000"
    ) +
    ggplot2::theme_minimal()
}
