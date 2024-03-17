#' Filter Classifications
#'
#' @param data
#' @param window_size
#' @param group
#'
#' @import dplyr
#' @return
#' @export
#'
#' @examples
filter_classifications <- function(
    data,
    window_size,
    group){
  # Remove small clusters
  # Could be improved with a way to collapse clusters together msartly if there are multiple adjacent ones
  state_summary <- data |>
    dplyr::group_by(group, .data[[state_number]]) |>
    dplyr::summarise(state_duration = n()/(fps*60),
              state = mean(state, na.rm = TRUE)) |>
    dplyr::mutate(gap_change = dplyr::if_else(state_duration > transition_window, 0, -1),
           gap_change = dplyr::if_else(is.na(gap_change), 0, gap_change),
           cum_gap_change = cumsum(gap_change),
           temp_state_number = state_number + cum_gap_change) |>
    dplyr::group_by(group, temp_state_number) |>
    dplyr::mutate(new_state = dplyr::first(state)) |>
    dplyr::ungroup() |>
    group_by(group) |>
    dplyr::mutate(new_gap_change = dplyr::if_else(new_state != dplyr::lag(new_state), 1, 0),
           new_gap_change = dplyr::if_else(is.na(new_gap_change), 0, new_gap_change),
           new_state_number = cumsum(new_gap_change)) |>
    dplyr::select(animal_id, state_number, new_state, new_state_number)


  # Join the correct states numbers back in
  grouping_var <- dplyr::enquo(group)
  data <- data |>
    dplyr::left_join(state_summary, by = c(grouping_var, "state_number")) |>
    dplyr::mutate(state_number = new_state_number,
           state = new_state
    ) |>
    dplyr::select(-new_state_number, -new_state) |>
    dplyr::mutate(state = dplyr::if_else(state < 1, 0, 1))

  return(data)
}
