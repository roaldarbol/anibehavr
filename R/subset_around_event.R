#' Subset Around Event
#'
#' @param data Data frame
#' @param var Variable to center around
#' @param from Reference point (e.g. "Day"/"Night")
#' @param time_var Which variable represents time
#' @param time_radius Time around point
#'
#' @return Subset of data frame around point in time
#' @export

subset_around_event <- function(data, var, from, time_var, time_radius = FALSE) {
  # Some symbolic pointers and other annoying stuff...
  var <- substitute(var)
  var_lag <- paste0(as_label(var),"_lag")
  var_lag_sym <- sym(var_lag)

  time_var <- substitute(time_var)
  time_var_sym <- as_label(time_var)

  max_time <- data %>%
    ungroup() %>%
    summarise(max_time = max({{ time_var }})) %>%
    as_vector()

  # Find the right time
  switch_time <- data %>%
    mutate( !!var_lag_sym := lag( {{ var }} )) %>%
    filter(
      {{ var }} != from,
      {{ var_lag_sym }} == from
      ) %>%
    ungroup() %>%
    select({{ time_var }}) %>%
    distinct() %>%
    filter({{ time_var }} > hms(minutes = time_radius + 5) &
           {{ time_var }} < max_time + hms(minutes = time_radius + 5))

  # Now get the data
  switch_data_all <- tibble()
  switch_data <- data
  rm(data)

  for (i in 1:nrow(switch_time)){
    switch_i <- switch_time[[i, time_var]]
    if (time_radius != FALSE){
      switch_data <- switch_data %>%
        filter({{ time_var }} >= switch_i - hms(minutes = time_radius) &
                 {{ time_var }} <= switch_i + hms(minutes = time_radius))
    }
    switch_data <- switch_data %>%
      mutate(rel_time = difftime({{ time_var }}, switch_i))
    switch_data_all <- bind_rows(switch_data_all, switch_data)
  }
  rm(switch_data)

  return(switch_data_all)
}

