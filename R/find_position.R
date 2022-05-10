#' Find position
#'
#' @param df track_list of data frames.
#' @param exp_setup Experimental setup, eith "wellplate " or "tube".
#' @param animal_ids Vector of animal IDs, as strings.
#'
#' @import dplyr
#' @importFrom forcats as_factor
#' @importFrom tidyr unite
#' @return A single tibble
#' @export


find_position <- function(df,
                          exp_setup = c("wellplate", "tube"),
                          animal_ids){

  if (exp_setup == "wellplate") {
    mean_xy <- summarise(df,
                         x_min = min(.data$x_cm),
                         x_max = max(.data$x_cm),
                         x_range = max(.data$x_cm) - min(.data$x_cm),
                         x_center = x_min + x_range / 2,
                         y_min = min(.data$y_cm),
                         y_max = max(.data$y_cm),
                         y_range = max(.data$y_cm) - min(.data$y_cm),
                         y_center = y_min + y_range / 2)

    # Break right
    df_temp <- df %>%
      filter(x_cm > mean_xy$x_center,
             x_cm < mean_xy$x_max)
    x_hist <- hist(df_temp$x_cm)
    x_min_right <- which.min(x_hist$density)
    break_right <- x_hist$mids[x_min_right]

    # Break left
    df_temp <- df %>%
      filter(x_cm < mean_xy$x_center,
             x_cm > mean_xy$x_min)
    x_hist <- hist(df_temp$x_cm)
    x_min_left <- which.min(x_hist$density)
    break_left <- x_hist$mids[x_min_left]

    # Break vertical
    df_temp <- df %>%
      filter(y_cm < mean_xy$y_max,
             y_cm > mean_xy$y_min)
    y_hist <- hist(df_temp$y_cm)
    y_min <- which.min(y_hist$density)
    break_y <- y_hist$mids[y_min]

    df <- df %>%
      mutate(height = if_else(.data$y_cm > break_y, "top", "bottom"),
             length = case_when(.data$x_cm < break_left ~ "left",
                                .data$x_cm > break_right ~ "right",
                                .data$x_cm > break_left & .data$x_cm < break_right ~ "middle")) %>%
      arrange(desc(.data$height), .data$length) %>%
      unite("position", .data$height:.data$length, remove=TRUE)

  } else if (exp_setup == "tube") {
    mean_xy <- summarise(df,
                         x_min = min(.data$x_cm),
                         x_max = max(.data$x_cm),
                         x_range = max(.data$x_cm) - min(.data$x_cm),
                         x_center = x_min + x_range / 2)
    df <- df %>%
      mutate(position = case_when(.data$x_cm < mean_xy$x_center - mean_xy$x_range/4 ~ "pos1",
                                .data$x_cm < mean_xy$x_center & .data$x_cm > mean_xy$x_center - mean_xy$x_range/4 ~ "pos2",
                                .data$x_cm > mean_xy$x_center & .data$x_cm < mean_xy$x_center + mean_xy$x_range/4 ~ "pos3",
                                .data$x_cm > mean_xy$x_center + mean_xy$x_range/4 ~ "pos4"
                                ))
  } else {
    stop("No experimental setup given")
  }

  # Do the rest of the fiddling
  positions <- unique(df$position)
  new_animal_ids <- na.omit(animal_ids)
  df$actual_id <- NA

  for (i in 1:length(positions)) {
    df[df$position == positions[[i]],][["actual_id"]] <- new_animal_ids[[i]]
  }

  df <- df %>%
    mutate(animal_id = as.factor(actual_id)) %>%
    select(-actual_id) %>%
    filter(!is.na(animal_id))
  return(df)
}
