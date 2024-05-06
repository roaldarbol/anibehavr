#'Generate bout numbers for binary variable
#'
#' @param data Data frame
#' @param var Variable name (in quotes)
#' @param .keep Keep both intermediate filter components (forward/backward)
#'
#' @import dplyr
#' @return Data frame with classifications
#' @export
generete_state_numbers <- function(
    data,
    var,
    .keep = FALSE){

  # Generate dynamic names
  var <- sym(var)
  state_change <- sym(paste(var, "change", sep = "_"))
  state_number <- sym(paste(var, "number", sep = "_"))

  data <- data |>
    dplyr::mutate(
      "{{ state_change }}" := dplyr::if_else({{ var }} != dplyr::lag({{ var }}), 1, 0),
      "{{ state_change }}" := dplyr::if_else(is.na({{ state_change }}), 0, {{ state_change }}),
      "{{ state_number }}" := cumsum({{ state_change }})
    ) |>
    select(-{{ state_change }})

  return(data)
}
