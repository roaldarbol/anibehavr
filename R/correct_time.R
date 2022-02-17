#' Correct time
#'
#' @param data
#'
#' @return
#' @export
#'
correct_time <- function(data) {
  n_vids <- unique(data$vid)
  for (i in 2:length(n_vids)) {
    max_time_vid <- data %>%
      filter(.data$vid == i-1) %>%
      summarise(max_time_vid = max(.data$time)) %>%
      as.numeric()

    data <- data %>%
      mutate(time = if_else(.data$vid == i,
                            .data$time + max_time_vid,
                            .data$time))
  }
  return(data)
}
