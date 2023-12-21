#' Min-max normalisation
#'
#' @param x
#'
#' @return Normalisation
#' @export
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
