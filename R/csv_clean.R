#' csv_clean
#'
#' @param df Raw TRex csv file
#'
#' @import dplyr
#' @importFrom janitor clean_names
#' @return df
#' @export
#'

csv_clean <- function(df){
  df <- df %>%
    janitor::clean_names() %>%
    select(.data$time,
           .data$x_number_wcentroid_cm,
           .data$y_number_wcentroid_cm,
           .data$speed_number_wcentroid_cm_s) %>%
    rename(x_cm = .data$x_number_wcentroid_cm,
           y_cm = .data$y_number_wcentroid_cm,
           speed = .data$speed_number_wcentroid_cm_s)
  return(df)
}
