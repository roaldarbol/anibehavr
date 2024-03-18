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
    window_widths,
    .keep = FALSE){

  for (i in 1:length(window_widths)){

    # Generate dynamic names
    n <- as.numeric(i)
    rolling_right_new <- sym(paste("rolling_right", n, sep = "_"))
    rolling_left_new <- sym(paste("rolling_left", n, sep = "_"))
    state <- sym(paste("state", n, sep = "_"))
    state_change <- sym(paste("state_change", n, sep = "_"))
    state_number <- sym(paste("state_number", n, sep = "_"))

    new_cols <- data |>
      group_modify(
        ~ filter_forward_backward(
          .x,
          movement_var = is_locomoting,
          window_width = 10)
      ) |>
      dplyr::mutate(
        "{{ rolling_right_new }}" := dplyr::if_else(rolling_right > 0, 1, 0),
        "{{ rolling_left_new }}" := dplyr::if_else(rolling_left > 0, 1, 0),
        "{{ state }}" := dplyr::if_else({{ rolling_right_new }} > 0 & {{ rolling_left_new }} > 0, 1, 0),
        "{{ state_change }}" := dplyr::if_else({{ state }} != dplyr::lag({{ state }}), 1, 0),
        "{{ state_change }}" := dplyr::if_else(is.na({{ state_change }}), 0, {{ state_change }}),
        "{{ state_number }}" := cumsum({{ state_change }})
      ) |>
      ungroup() |>
      select(last_col(4):last_col())
    data <- bind_cols(data, new_cols)
  }


  # Get rid of the filters themselves
  if (.keep == FALSE){
    data <- data |>
      select(-starts_with("rolling_"))
  }
  return(data)
}

