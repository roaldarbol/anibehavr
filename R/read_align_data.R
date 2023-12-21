#' Title
#'
#' @param filename Filenames
#' @param bin_seconds Bin duration in seconds
#'
#' @return Aligned data frame
#' @export
#'
read_align_data <- function(filename, bin_seconds){
  # Subset data 30 minutes pre/post a light switch
  lengths <- tibble()
  # cat("Processing", i)

  temp_data <- read_parquet(filename) %>%
    as_tibble() %>%
    mutate(hms_time = hms::hms(seconds = time)) %>%
    group_by(animal_id) %>%
    mutate(speed_sqrt = sqrt(speed)) %>%
    mutate(rel_speed = speed_sqrt / max(speed_sqrt, na.rm = TRUE))

  # Subset around light on/off
  if (unique(temp_data$light_condition) == "l:d" & "light" %in% colnames(temp_data)) {
    aligned_data <- temp_data %>%
      around_point(light, from = 0, time_var = hms_time, time_radius = FALSE) %>%
      mutate(plot_seconds = as.numeric(rel_time))
    rm(temp_data)
  } else {
    aligned_data <- temp_data %>%
      around_point(time_of_day, from = "night_pre", time_var = hms_time, time_radius = FALSE) %>%
      mutate(plot_seconds = as.numeric(rel_time))
    rm(temp_data)
  }

  # Shorten data
  meta <- aligned_data %>%
    select(c("date", "species", "light_condition")) %>%
    distinct()

  aligned_data_short <- aligned_data %>%
    compress_observations(n_observations = nrow(aligned_data) / (30 * bin_seconds),
                          group = animal_id) %>%
    mutate(rowid = row_number())
  rm(aligned_data)
  return(aligned_data_short)
}
