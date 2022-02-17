#' Compress observations
#'
#' Get an average of your data where the output is n rows (approximately).
#'
#' @name compress_observations
#' @param data Data
#' @param vars Variables to mean
#' @param n_observations Number of observations in output data frame
#' @param group grouping variable
#' @import dplyr
#' @importFrom utils globalVariables
#' @return
#' @export

utils::globalVariables("where")
compress_observations <- function(data, vars = where(is.numeric), n_observations, group = NULL) {
  data_short <- data %>%
    group_by({{ group }}) %>%
    mutate(G = trunc(2:(n()+1)/(nrow(data)/n_observations))) %>%
    ungroup() %>%
    group_by({{ group }}, .data$G) %>%
    summarise(across({{ vars }}, ~ mean(.x, na.rm = TRUE)),
              time = min(.data$time)) %>%
    ungroup() %>%
    select(-.data$G)
}
