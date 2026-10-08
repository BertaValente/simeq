
#' Harmonise BMI category labels
#'
#' Converts accepted BMI-category label variants to the standard BMI-category
#' labels used throughout simeq.
#'
#' @param x A character vector of BMI category labels.
#'
#' @return A character vector using standard simeq BMI category labels.
#'   Unsupported or missing labels return `NA_character_`.
#'
#' @export
simeq_harmonise_bmi_category <- function(x) {
  x_chr <- as.character(x)

  output <- rep(
    NA_character_,
    length(x_chr)
  )

  output[
    x_chr %in% c(
      "underweight",
      "Underweight",
      "under_weight"
    )
  ] <- "underweight"

  output[
    x_chr %in% c(
      "normal_weight",
      "normalweight",
      "normal weight",
      "Normal",
      "normal"
    )
  ] <- "normal_weight"

  output[
    x_chr %in% c(
      "overweight",
      "over_weight",
      "Overweight"
    )
  ] <- "overweight"

  output[
    x_chr %in% c(
      "obesity_I",
      "obesityclassI",
      "obesity I",
      "obesity class I",
      "Obesity I"
    )
  ] <- "obesity_I"

  output[
    x_chr %in% c(
      "obesity_II",
      "obesityclassII",
      "obesity II",
      "obesity class II",
      "Obesity II"
    )
  ] <- "obesity_II"

  output[
    x_chr %in% c(
      "obesity_III",
      "obesityclassIII",
      "obesity III",
      "obesity class III",
      "Obesity III"
    )
  ] <- "obesity_III"

  output
}
