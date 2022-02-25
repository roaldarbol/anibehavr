#' Sync time
#'
#' @param .data Data frame or tibble
#' @param x Parameter used to merge (often time)
#' @param y Variable to be synced along with x
#' @param data2 The second data frame
#'
#' @return Tibble
#' @export
sync_time <- function(.data, x, y, data2) {
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
