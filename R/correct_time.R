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
