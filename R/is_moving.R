#' is_moving
#'
#' @param df Data frame.
#' @param type Use mean or median.
#' @param n_frames Number of frames used to generate rolling average.
#'
#' @import dplyr
#' @return
#' @export
#'

is_moving <- function(df, type, n_frames = 50){
  # Add rolling mean to determine whether quiescent
  if("time" %in% colnames(df)){
    df_temp <- df %>%
      select(~time, ~speed)
    if (type=='mean'){
      df_roll <- as.data.frame(zoo::rollmean(df_temp, {{ n_frames }}))
    } else if (type =='median'){
      df_roll <- as.data.frame(zoo::rollmedian(df_temp, {{ n_frames }}))
    }

    names(df_roll) <- c('time', 'speed')

    #s <- zoo::rollmean(df$`SPEED#smooth#wcentroid`, 25)
    df_roll <- df_roll %>%
      mutate(moving = case_when(
        speed <= 0.25                  ~ 0,
        # speed > 0.25   & speed < 0.75   ~ 1,
        speed > 0.25                   ~ 1
      ))
  } else {
    warning('Variable time doesn\'t exist, returns empty df')
    df_roll <- data.frame()
  }
  return(df_roll)
}
