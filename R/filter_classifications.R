#' Filter Classifications
#'
#' @param data Data frame
#' @param window_widths Minimum duration of bout
#'
#' @import dplyr
#' @importFrom rlang sym
#' @return Filtered data frame
#' @export
filter_classifications <- function(
    data,
    window_widths
){
  data_groups <- dplyr::groups(data)
  n_states <- data |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::starts_with("state_number")) |>
    length()

  for (i in 1:n_states){
    n <- as.numeric(i)
    state <- rlang::sym(paste("state", n, sep = "_"))
    state_number <- rlang::sym(paste("state_number", n, sep = "_"))

    # Remove small clusters
    # Could be improved with a way to collapse clusters together mostly if there are multiple adjacent ones
    state_summary <- data |>
      dplyr::group_by({{ state_number }}, .add = TRUE) |>
      dplyr::summarise(
        state_duration = n(),
        state_binary = mean({{ state }}, na.rm = TRUE)
        ) |>
      dplyr::mutate(
        gap_change = dplyr::if_else(state_duration > window_widths[i], 0, -1),
        gap_change = dplyr::if_else(is.na(gap_change), 0, gap_change),
        cum_gap_change = cumsum(gap_change),
        temp_state_number = {{ state_number }} + cum_gap_change) |>
      dplyr::ungroup({{ state_number }}) |>
      dplyr::group_by(temp_state_number, .add = TRUE) |>
      dplyr::mutate(new_state = dplyr::first(state_binary)) |>
      dplyr::ungroup(temp_state_number) |>
      dplyr::mutate(new_gap_change = dplyr::if_else(new_state != dplyr::lag(new_state), 1, 0),
             new_gap_change = dplyr::if_else(is.na(new_gap_change), 0, new_gap_change),
             new_state_number = cumsum(new_gap_change)) |>
      dplyr::select(-gap_change, -cum_gap_change, -temp_state_number, -state_binary, -new_gap_change, -state_duration)


    # Join the correct states numbers back in
    state_number_char <- paste("state_number", n, sep = "_")
    new_data_groups <- c(as.character(data_groups), state_number_char)
    data <- dplyr::left_join(data, state_summary, by = new_data_groups) |>
      dplyr::mutate({{ state_number }} := new_state_number,
             {{ state }} := new_state,
             {{ state }} := if_else({{ state }} < 1, 0, 1)) |>
      dplyr::select(-new_state_number, -new_state)
  }

  return(data)
}
