#' Sync time
#'
#' @param .data Data frame or tibble
#' @param x Parameter used to merge (often time)
#' @param y Variable to be synced along with x
#' @param data2 The second data frame
#'
#' @return Tibble
#' @export
sync_time <- function(.data, data2, x, y) {
  as_tibble(
    approx(
      .data[[{{ x }}]],
      .data[[{{ y }}]],
      unique(data2[[{{ x }}]]),
      method="constant",
      rule=2
    )
  ) %>%
    rename(
      {{ x }} := x,
      {{ y }} := y)
}

#' @export
#' @rdname join_timeseries
join_timeseries <- function(
    .x,
    y,
    by = NULL,
    group = NULL,
    method = NA,
    copy = FALSE,
    suffix = c("", ".y"),
    keep = NULL) {

  # Start out by creating all the grouping - data should have all the relevant times for each group.
  tbl_group_vars <- group_vars(.x)

  if (length({{ tbl_group_vars }}) > 0){
    grouped_x <- .x %>%
      group_split()
    grouped_y <- y %>%
      group_split()
  } else {
    grouped_x <- list(.x)
    grouped_y <- list(y)
  }

  output_list <- list()
  for (i in 1:length(grouped_x)){
    current_x <- grouped_x[[i]]
    current_y <- grouped_y[[i]]
    by_x <- current_x[[{{ by }}]]
    by_y <- current_y[[{{ by }}]]
    by_both <- c(by_x, by_y) %>%
      as_tibble() %>%
      arrange(value) %>%
      distinct() %>%
      rename(
        {{ by }} := value
      )

    current_combined <- by_both %>%
      left_join(current_x, by = {{ by }}) %>%
      left_join(current_y, by = {{ by }}, suffix = {{ suffix }})

    # Finish grouping
    if (length({{ tbl_group_vars }}) > 0){
      # Find unique grouping level
      group_level <- current_combined %>%
        select({{ tbl_group_vars }}) %>%
        distinct() %>%
        na.omit() %>%
        pull()

      # Replace NAs in grouping column
      current_combined <- current_combined %>%
        mutate({{ tbl_group_vars }} := {{ group_level }})


      # Remove extra group column
      current_combined <- current_combined %>%
        select(-paste0({{ tbl_group_vars }}, ".y"))
    }

    output_list[[i]] <- current_combined
  }

  output_tibble <- bind_rows(output_list)
  return(output_tibble)
}
