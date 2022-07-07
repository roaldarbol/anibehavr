#' Title
#'
#' @param filenames Filenames
#' @param bin_seconds Bin duration in seconds
#'
#' @return
#' @export
#'
read_align_all_data <- function(filenames, bin_seconds){
  # Subset data 30 minutes pre/post a light switch
  aligned_data_short_all <- tibble()
  lengths <- tibble()
  cli_progress_bar(
    "Reading data",
    total = length(filenames),
    format = "{cli::pb_bar} {pb_percent} @ {basename(i)}"
    )
  for (i in filenames) {
    cli_progress_update()
    # cat("Processing", i)

    temp_data <- read_parquet(i) %>%
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
        around_point(time_of_day, from = "Night", time_var = hms_time, time_radius = FALSE) %>%
        mutate(plot_seconds = as.numeric(rel_time))
      rm(temp_data)
    }

    # Shorten data
    meta <- aligned_data %>%
      select(c("date", "species", "light_condition")) %>%
      distinct()

    aligned_data_short <- aligned_data %>%
      # group_by(animal_id) %>%
      # filter(row_number() %% (30 * bin_seconds) == 1) %>%
      compress_observations(n_observations = nrow(aligned_data) / (30 * bin_seconds),
                            group = animal_id) %>%
      mutate(rowid = row_number(),
             date = meta$date,
             species = meta$species,
             light_condition = meta$light_condition)
    rm(aligned_data)
    aligned_data_short_all <- bind_rows(aligned_data_short_all, aligned_data_short)
    rm(aligned_data_short)
  }
  cli_progress_done()
  return(aligned_data_short_all)
}
