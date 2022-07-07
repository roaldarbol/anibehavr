#' IDENTIFY PERIODS OF INTEREST
#'
#' @param data Data frame
#' @param var Variable to be identified
#' @param newvar New variable name
#' @param threshold Method for thresholding. Either "minmax", "mediansd" or a number.
#' @param as_factor Whether an output should be returned as factor
#'
#' @return
#' @export

identify_periods <- function(data, var, newvar, threshold = "minmax", as_factor = FALSE) {

  var.name <- substitute(var)
  if (is.numeric(threshold)) {
    half <- threshold
  } else if (threshold == "minmax") {
    half <- min(data[var.name], na.rm = TRUE) + ((max(data[var.name], na.rm = TRUE) - min(data[var.name], na.rm = TRUE)) / 2)
  } else if (threshold == "mediansd") {
    half <- median(data[[var.name]], na.rm = TRUE) + sd(data[[var.name]], na.rm = TRUE)
  }

  if (as_factor == FALSE) {
    data <- data %>%
      mutate({{ newvar }} := if_else({{ var }} > half, 1, 0))
  } else if (as_factor == TRUE) {
    data <- data %>%
      mutate({{ newvar }} := as_factor(if_else({{ var }} > half, 1, 0)))
  }

  return(data)
}
