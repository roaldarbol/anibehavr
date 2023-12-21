#' Non-numeric mode
#'
#' @param x Vector to find the mode over
#'
#' @return Mode
#' @export
#'

mode_nonnumeric <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
