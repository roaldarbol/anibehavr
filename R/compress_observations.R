#' Compress observations
#'
#' Get an average of your data where the output is n rows (approximately).
#'
#' @name compress_observations
#' @param data Data
#' @param n_observations Number of observations in output data frame
#' @param group grouping variable
#' @import dplyr
#' @importFrom utils globalVariables
#' @return Smaller data frame where values are means of N observations
#' @export

utils::globalVariables("where")
compress_observations <- function(data, n_observations, group = NULL) {
  data_short <- data %>%
    group_by({{ group }}) %>%
    mutate(G = trunc(2:(n()+1)/(nrow(data)/n_observations))) %>%
    ungroup() %>%
    drop_na() %>%
    group_by({{ group }}, .data$G) %>%
    summarise(
      across(where(is.numeric), ~ mean(., na.rm = TRUE)),
      across(where(negate(is.numeric)), ~ mode_nonnumeric(.)),
      time = min(.data$time)
      ) %>%
    ungroup() %>%
    select(-.data$G)
}
