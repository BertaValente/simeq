utils::globalVariables(
  c(
    "year",
    "mean_qaly_per_person",
    "obesity_status",
    "qaly_lo",
    "qaly_hi"
  )
)

#' Plot QALY trajectories by obesity status
#'
#' Creates annual QALY trajectories by obesity status, with 95% simulation
#' interval ribbons. Panels are stratified by sex and education level.
#'
#' @param qaly_results A list returned by [ponte_calculate_qalys()].
#'
#' @return A ggplot object.
#'
#' @export
ponte_plot_qaly_trajectory <- function(
    qaly_results
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package `ggplot2` is required for plotting. ",
      "Install it with `install.packages(\"ggplot2\")`.",
      call. = FALSE
    )
  }

  if (
    !is.list(qaly_results) ||
    is.null(qaly_results$by_year_obesity)
  ) {
    stop(
      "`qaly_results` must be a list containing `by_year_obesity`.",
      call. = FALSE
    )
  }

  trajectory_data <- qaly_results$by_year_obesity

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
    names(trajectory_data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`qaly_results$by_year_obesity` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  ggplot2::ggplot(
    trajectory_data,
    ggplot2::aes(
      x = year,
      y = mean_qaly_per_person,
      colour = obesity_status,
      fill = obesity_status
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = qaly_lo,
        ymax = qaly_hi
      ),
      alpha = 0.20,
      colour = NA
    ) +
    ggplot2::geom_line(
      linewidth = 0.8
    ) +
    ggplot2::facet_grid(
      sex ~ education_level
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        no_obesity = "#0072B2",
        obesity = "#D55E00"
      ),
      name = "Obesity status"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        no_obesity = "#0072B2",
        obesity = "#D55E00"
      ),
      name = "Obesity status"
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Mean QALYs per person"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    )
}
