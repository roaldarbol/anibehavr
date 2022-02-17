#' inverse_ecdf
#'
#' @param data A data frame (e.g. a tibble)
#' @param col Variable to make the density function for.
#' @param min_length Minimal starting duration in frames
#' @param rest_state Whether to make the ecdf for resting or active periods. TRUE or FALSE (default)
#'
#' @return
#' @export

inverse_ecdf <- function(data, col, min_length, rest_state = FALSE) {

  col_subset <- data %>%
    select({{ col }}) %>%
    # select(moving) %>%
    pull()

  check <- rle(col_subset)
  long <- which(check$lengths > min_length)
  still <- which(check$values == rest_state)
  long_still <- check$lengths[intersect(long, still)] %>%
    as_tibble() %>%
    rename(rest_duration = value) %>%
    arrange(desc(rest_duration)) %>%
    add_count(rest_duration)

  rest_probability <- long_still %>%
    summarise(prop = cume_dist(-rest_duration)) %>%
    bind_cols(long_still) %>%
    distinct() %>%
    relocate(n, .before = prop)

  # rest_probability2 <- long_still %>%
  #   count(rest_duration) %>%
  #   bind_cols(long_still) %>%
  #   distinct()

  return(rest_probability)
}
