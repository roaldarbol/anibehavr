#' Title
#'
#' @param filenames Filenames
#' @param bin_seconds Bin duration in seconds
#' @param time_radius Radius around change
#'
#' @return
#' @export
#'
read_switch_data <- function(filenames, bin_seconds, time_radius=FALSE){
  # Subset data 30 minutes pre/post a light switch
  light_switch_all <- tibble()
  light_switch_short_all <- tibble()
  lengths <- tibble()
  cli_progress_bar("Reading data", total = length(filenames))
  for (i in filenames) {
    temp_data <- read_parquet(i) %>%
      as_tibble() %>%
      mutate(hms_time = hms::hms(seconds = time)) %>%
      group_by(animal_id) %>%
      mutate(rel_speed = sqrt(speed) / max(sqrt(speed), na.rm = TRUE))

    # Subset around light on/off
    if (length(unique(temp_data$light)) == 2) {
      light_switch_on <- temp_data %>%
        around_point(light, from = 0, time_var = hms_time, time_radius = time_radius) %>%
        mutate(light_switch = "on")
      light_switch_off <- temp_data %>%
        around_point(light, from = 1, time_var = hms_time, time_radius = time_radius) %>%
        mutate(light_switch = "off")

      # For debugging ----
      # tt <- bind_rows(light_switch_off, light_switch_on)
      # test <- tt %>%
      #   group_by(animal_id, date, light_switch) %>%
      #   summarise(n = n())
      # lengths <- bind_rows(lengths, test)
      # ----

      # Shorten data
      light_switch <- bind_rows(light_switch_on, light_switch_off)
      light_switch_short <- light_switch %>%
        group_by(animal_id, date, light_switch) %>%
        filter(row_number() %% (30 * bin_seconds) == 1) %>%
        mutate(rowid = row_number()) %>%
        ungroup() %>%
        mutate(new_time = as.numeric(rel_time))

      light_switch_short_all <- bind_rows(light_switch_short_all, light_switch_short)
      # light_switch_all <- bind_rows(light_switch_all, light_switch_on, light_switch_off)
      rm(light_switch_on, light_switch_off, light_switch, light_switch_short)
      cli_progress_update()
    }
    rm(temp_data)
  }
  return(light_switch_short_all)
}
