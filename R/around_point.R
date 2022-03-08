#' Subset around point
#'
#' @param data Data frame
#' @param var Variable to center around
#' @param ref_condition Reference point (e.g. "Day"/"Night")
#' @param time_radius Time around point
#'
#' @return
#' @export

around_point <- function(data, var, ref_condition, time_radius) {
  var <- quo(light)
  var_lag <- paste0(as_label(var),"_lag")

  on <- data %>%
    mutate(!! var_lag := lag( !! var )) %>%
    filter(
      !! rlang::sym(as_label(var)) == ref_condition,
      !! rlang::sym(var_lag) != ref_condition) %>%
    ungroup() %>%
    select(hms_time) %>%
    distinct() %>%
    filter(hms_time > hms(minutes = time_radius + 5) &
             hms_time < max(data$hms_time, na.rm = TRUE) + hms(minutes = time_radius + 5))

  off <- data %>%
    mutate(!! var_lag := lag( !! var )) %>%
    filter(
      !! rlang::sym(as_label(var)) != ref_condition,
      !! rlang::sym(var_lag) == ref_condition) %>%
    ungroup() %>%
    select(hms_time) %>%
    distinct() %>%
    filter(hms_time > hms(minutes = time_radius + 5) &
             hms_time < max(data$hms_time, na.rm = TRUE) + hms(minutes = time_radius + 5))

  switch_on <- data %>%
    filter(hms_time >= on$hms_time[1] - hms(minutes = time_radius) &
             hms_time <= on$hms_time[1] + hms(minutes = time_radius)) %>%
    mutate(switch = "on",
           rel_time = difftime(hms_time, on$hms_time[1]))

  switch_off <- data %>%
    filter(hms_time >= off$hms_time[1] - hms(minutes = time_radius) &
             hms_time <= off$hms_time[1] + hms(minutes = time_radius)) %>%
    mutate(switch = "off",
           rel_time = difftime(hms_time, off$hms_time[1]))

  switch_all <- bind_rows(switch_on, switch_off)
  return(switch_all)
}
