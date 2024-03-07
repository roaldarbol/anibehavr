is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))
