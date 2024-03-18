#' Title
#'
#' @param filename Filenames
#' @param bin_seconds Bin duration in seconds
#'
#' @import dplyr
#' @importFrom hms hms
#'
#' @return Aligned data frame
#' @export
#'
read_align_data <- function(data, bin_seconds, time, animal_id){
  # Subset data 30 minutes pre/post a light switch
  lengths <- dplyr::tibble()
  # cat("Processing", i)

  temp_data <- data |>
    dplyr::as_tibble() |>
    dplyr::mutate(hms_time = hms::hms(seconds = time)) |>
    dplyr::group_by(animal_id) |>
    dplyr::mutate(speed_sqrt = sqrt(speed)) |>
    dplyr::mutate(rel_speed = speed_sqrt / max(speed_sqrt, na.rm = TRUE))

  # Subset around light on/off
  if (unique(temp_data$light_condition) == "l:d" & "light" %in% colnames(temp_data)) {
    aligned_data <- temp_data |>
      subset_around_event(light, from = 0, time_var = hms_time, time_radius = FALSE) |>
      mutate(plot_seconds = as.numeric(rel_time))
    rm(temp_data)
  } else {
    aligned_data <- temp_data |>
      subset_around_event(time_of_day, from = "night_pre", time_var = hms_time, time_radius = FALSE) |>
      mutate(plot_seconds = as.numeric(rel_time))
    rm(temp_data)
  }

  # Shorten data
  meta <- aligned_data |>
    select(c("date", "species", "light_condition")) |>
    distinct()

  aligned_data_short <- aligned_data |>
    compress_observations(n_observations = nrow(aligned_data) / (30 * bin_seconds),
                          group = animal_id) |>
    mutate(rowid = row_number())
  rm(aligned_data)
  return(aligned_data_short)
}
