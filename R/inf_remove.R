#' Title
#'
#' @param df Data frame
#'
#' @importFrom purrr is_empty
#' @return
#' @export
#'

inf_remove <- function(df){
  bad <- which(!is.finite(as.matrix(df)), arr.ind=TRUE)
  bad <- unique(bad[,1])
  bad.plus <- bad+1
  if (length(bad)!=nrow(df) & !purrr::is_empty(bad)){
    df <- df[-bad.plus,]
    df <- df[!rowSums(!is.finite(as.matrix(df))),]
  }
  return(df)
}
