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

join_timeseries <- function(
    .x,
    y,
    by = NULL,
    method = NA,
    copy = FALSE,
    suffix = c(".x", ".y"),
    ...,
    keep = NULL) {

  by_x <- .x[[{{ by }}]]
  by_y <- y[[{{ by }}]]
  by_both <- c(by_x, by_y) %>%
    as_tibble() %>%
    arrange(value) %>%
    distinct() %>%
    rename(
      {{ by }} := value
    )

  output_tibble <- by_both %>%
    left_join(.x, by = {{ by }}) %>%
    left_join(y, by = {{ by }}, suffix = {{ suffix }})

  if (!is.na({{ method }})){

  }

  return(output_tibble)
}
