#' csv_clean
#'
#' @param df Raw TRex csv file
#'
#' @import dplyr
#' @importFrom janitor clean_names
#' @importFrom tidyselect where
#' @return df
#' @export
#'

csv_clean <- function(df){
  df <- df %>%
    janitor::clean_names() %>%
    select(time, x_number_wcentroid_cm, y_number_wcentroid_cm, speed_number_wcentroid_cm_s) %>%
    rename(x_cm = x_number_wcentroid_cm,
           y_cm = y_number_wcentroid_cm,
           speed = speed_number_wcentroid_cm_s)
  return(df)
}
