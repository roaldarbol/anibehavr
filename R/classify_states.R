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
    group = "animal_id"){

  group_var <- enquo(group)

  # Some data.table magic to find minimum values in the classification in specified time windows
  # Rather than using rolling minimum (which is *incredibly* slow), we use a rolling sum and an "if_else(sum>0)"-approach
  data.table::setDT(data)
  data[, rolling_right := data.table::frollsum(movement_var, n = window_width, algo = "fast", align = "right"), by = group_var]
  data[, rolling_left := data.table::frollsum(movement_var, n = window_width, algo = "fast", align = "left"), by = group_var]
  data.table::setDF(data)

  # Compute state
  # The state is high only when both filters are high
  data <- data |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      rolling_right = dplyr::if_else(rolling_right > 0, 1, 0),
      rolling_left = dplyr::if_else(rolling_left > 0, 1, 0),
      state = dplyr::if_else(rolling_right > 0 & rolling_left > 0, 1, 0),
      state_change = dplyr::if_else(state != dplyr::lag(state), 1, 0),
      state_change = dplyr::if_else(is.na(state_change), 0, state_change),
      state_number = cumsum(state_change)
      )
  return(data)
}
