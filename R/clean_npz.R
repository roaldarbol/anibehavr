npz_clean <- function(df.npz){
  r <- data.frame(name=NA, n=NA)
  for (i in 1:length(df.npz$files)){
    a <- df.npz$files[[i]]
    b <- nrow(df.npz$f[[df.npz$files[[i]]]])
    r <- rbind(r, c(a,b))
  }

  # Filter out columns of different length
  r <- r %>%
    filter(.data$name != 'segment_vxys' & .data$name != 'frame_segments')

  # Make data.frame
  rown <- as.integer(r[[1,2]])
  df <- tibble(.rows = rown)
  for (i in 1:nrow(r)){
    df <- cbind(df, df.npz$f[[r$name[i]]])
  }
  names(df) <- r$name
  df <- df[-(1:50),]
  return(df)
}
