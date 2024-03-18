#' Classify states Across Time-scales
#'
#' @param data Data frame
#' @param window_width Window width in number of frames
#' @param movement_var
#' @param group
#'
#' @import data.table
#' @import dplyr
#' @importFrom rlang enquo
#' @return
#' @export
#'
#' @examples
classify_states <- function(
    data,
    movement_var,
    window_width,
    .keep = FALSE){

  data <- data |>
    group_modify(
      ~ filter_forward_backward(
        .x,
        movement_var = movement_var,
        window_width = window_width)
      )

  # Compute state
  # The state is high only when both filters are high
  data <- data |>
    dplyr::mutate(
      rolling_right = dplyr::if_else(rolling_right > 0, 1, 0),
      rolling_left = dplyr::if_else(rolling_left > 0, 1, 0),
      state = dplyr::if_else(rolling_right > 0 & rolling_left > 0, 1, 0),
      state_change = dplyr::if_else(state != dplyr::lag(state), 1, 0),
      state_change = dplyr::if_else(is.na(state_change), 0, state_change),
      state_number = cumsum(state_change)
      )

  # Get rid of the filters themselves
  if (.keep == FALSE){
    data <- data |>
      select(-rolling_right, -rolling_left)
  }
  return(data)
}

