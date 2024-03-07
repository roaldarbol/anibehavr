#' csv_clean
#'
#' @param df Raw TRex csv file
#'
#' @import dplyr
#' @importFrom janitor clean_names
#' @return df
#' @export
#'

csv_clean <- function(df, tracker = c("trex", "idtrackerai")){
  if (tracker == "trex"){
    df <- df %>%
      janitor::clean_names() |>
      select(.data$time,
             .data$x_number_wcentroid_cm,
             .data$y_number_wcentroid_cm,
             .data$speed_number_wcentroid_cm_s) |>
      rename(x_cm = .data$x_number_wcentroid_cm,
             y_cm = .data$y_number_wcentroid_cm,
             speed = .data$speed_number_wcentroid_cm_s)
  } else if (tracker == "idtrackerai"){
    df <- df %>%
      janitor::clean_names() |>
      pivot_longer(cols = 2:ncol(df),
                   values_to = "value",
                   names_to = c("axis", "id"),
                   names_pattern = "(.)(.)") |>
      mutate(id = as.factor(id)) |>
      pivot_wider(id_cols = c("id", "seconds"),
                  values_from = "value",
                  names_from = "axis") |>
      rename(time = seconds) |>
      group_by(id) |>
      mutate(speed = sqrt((x-lag(x))^2 + (y-lag(y))^2) / (time-lag(time)))

    df[is.nan(df)] <- NA
  }
  return(df)
}
