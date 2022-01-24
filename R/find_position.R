#' Find position
#'
#' @param track_list track_list of data frames.
#' @param exp_setup Experimental setup, eith "wellplate " or "tube".
#' @param animal_ids Vector of animal IDs, as strings.
#'
#' @import dplyr
#' @importFrom forcats as_factor
#' @importFrom tidyr unite
#' @return
#' @export


find_position <- function(track_list,
                          exp_setup = c("wellplate", "tube"),
                          animal_ids){
  for (i in 1:(length(track_list)/6)){
    for (j in 1:6){
      track_list[[j+(i-1)*6]][["id"]] <- as.factor(j)
      track_list[[j+(i-1)*6]][["vid"]] <- as.factor(i)
    }
  }

  df <- bind_rows(track_list)
  if (exp_setup == "wellplate") {
    mean_xy <- summarise(df,
                         x_mean = mean(x_cm),
                         x_range = max(x_cm) - min(x_cm),
                         y_mean = mean(y_cm),
                         y_range = max(y_cm) - min(y_cm))
    df <- df %>%
      group_by(~id, ~vid) %>%
      summarise(x = mean(x_cm),
                y = mean(y_cm)) %>%
      mutate(height = if_else(~y > mean_xy$y_mean, "top", "bottom"),
             length = case_when(x < mean_xy$x_mean - mean_xy$x_range/6 ~ "left",
                                x > mean_xy$x_mean + mean_xy$x_range/6 ~ "right",
                                x > mean_xy$x_mean - mean_xy$x_range/6 & x < mean_xy$x_mean + mean_xy$x_range/6 ~ "middle")) %>%
      arrange(desc(height), length) %>%
      unite("position", height:length, remove=TRUE) %>%
      select(~id, ~vid, ~position)

    positions <- unique(df$position)
    df$actual_id <- NA

    for (i in 1:length(positions)) {
      df[df$position == positions[[i]],][["actual_id"]] <- animal_ids[[i]]
    }

    for (i in 1:length(track_list)) {
      id_vid <- track_list[[i]] %>%
        slice(1) %>%
        select(~id, ~vid)

      df_number <- which(df$id == levels(id_vid$id) &
                           df$vid == levels(id_vid$vid))
      track_list[[i]] <- track_list[[i]] %>%
        mutate(animal_id = as_factor(df$actual_id[[df_number]]))
    }

  }
  return(track_list)
}
