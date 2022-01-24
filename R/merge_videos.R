#' Merge videos
#'
#' Merges data sets for consecutive videos for each animal.
#'
#' @param df_list List of data frames
#'
#' @import dplyr
#' @importFrom utils tail
#' @return
#' @export
#'

merge_videos <- function(df_list) {
  for (i in 1:length(df_list)) {
    max_time <- utils::tail(df_list[[i]]$time, 1)
    if (levels(df_list[[i]]$vid) == 2) {
      df_list[[i]] <- df_list[[i]] %>%
        mutate(time = ~time + max_time)
    }
  }

  df <- bind_rows(df_list) %>%
    arrange(id, time)
}
