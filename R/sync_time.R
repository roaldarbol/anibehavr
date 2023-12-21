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

#' Ensure that grouping levels match across data frames - add an error/warning if not.
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
  tbl_group_vars_y <- group_vars(y)

  # Throw error if the data frames uses different groupings
  if (!identical(tbl_group_vars, tbl_group_vars_y)){
    stop("Data frames are grouped by different variables")
  }

  if (length({{ tbl_group_vars }}) > 0){
    # Only keep group keys with observations in both data frames
    keys_x <- .x %>%
      group_keys()
    keys_y <- y %>%
      group_keys()
    keys_both <- merge(keys_x, keys_y)
    .x <- inner_join(.x, keys_both, by = {{ tbl_group_vars }})
    y <- inner_join(y, keys_both, by = {{ tbl_group_vars_y }})

    # If there are no matches, give an informative error message
    if (nrow(keys_both) == 0){
      stop("Found no matches between groups. Check the grouping variables in both data frames.")
    }

    # Split by groups
    grouped_x <- .x %>%
      group_split()
    grouped_y <- y %>%
      group_split()
  } else {
    grouped_x <- list(.x)
    grouped_y <- list(y)
  }

  # We join for each individual grouping
  output_list <- list()
  for (i in 1:length(grouped_x)){
    if (length({{ tbl_group_vars }}) > 0){
      combinations <- c(tbl_group_vars, {{ by }})
      current_xy <- full_join(grouped_x[[i]], grouped_y[[i]], by = combinations)
      output_list[[i]] <- current_xy
    } else {
      current_xy <- full_join(grouped_x[[i]], grouped_y[[i]], by = {{ by }})
      output_list[[i]] <- current_xy
    }
  }

  # Then we bind to a tibble and arrange by grouping and {{ by }} (time)
  if (length({{ tbl_group_vars }}) > 0){
    output_tibble <- bind_rows(output_list) %>%
      arrange(!!! rlang::syms(c({{ combinations }})))
  } else {
    output_tibble <- bind_rows(output_list)
    output_tibble <- output_tibble %>%
      arrange(!! rlang::sym(c({{ by }})))
    }
  return(output_tibble)
}
