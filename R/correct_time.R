#' Correct time
#'
#' @param data
#'
#' @import data.table
#' @return A data frame where times are added.
#' @export
#'
correct_time <- function(data) {
  n_vids <- unique(na.omit(data$vid))
  if (length(n_vids) >= 2){
    for (i in 2:length(n_vids)) {
      max_time_vid <- data %>%
        filter(.data$vid == i-1) %>%
        summarise(max_time_vid = max(.data$time, na.rm = TRUE)) %>%
        as.numeric()

      data <- data %>%
        mutate(time = if_else(.data$vid == i,
                              .data$time + max_time_vid,
                              .data$time))
    }
  }
  return(data)
}


correct_time2 <- function(data) {
  n_vids <- unique(na.omit(data$vid))
  data.table::setDT(data)
  if (length(n_vids) >= 2){
    for (i in 2:length(n_vids)) {
      max_time_vid <- data[which(vid==i-1), max(time)]
      data[which(vid==i), time := time + max_time_vid]
    }
  }
  data.table::setDF(data)
  return(data)
}
