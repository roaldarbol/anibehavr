#' Forward-backward Filter
#'
#' @param data Data frame
#' @param movement_var
#' @param window_width
#' @param group
#'
#' @return
#' @export
#'
#' @examples
filter_forward_backward <- function(data, movement_var, window_width){
  movement_var <- substitute(movement_var)

  # Some data.table magic to find minimum values in the classification in specified time windows
  # Rather than using rolling minimum (which is *incredibly* slow), we use a rolling sum and an "if_else(sum>0)"-approach
  data.table::setDT(data)
  data[, rolling_right := data.table::frollsum(data[[ movement_var ]], n = window_width, algo = "fast", align = "right")]
  data[, rolling_left := data.table::frollsum(data[[ movement_var ]], n = window_width, algo = "fast", align = "left")]
  data.table::setDF(data)
  return(data)
}
