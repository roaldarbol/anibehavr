#' Assign video
#'
#' @param track_list List from video files
#' @param animal_ids Animal ids
#'
#' @return track_list
#' @export

assign_video <- function(track_list, animal_ids) {
  n_animals <- animal_ids %>%
    na.omit() %>%
    length()

  for (i in 1:(length(track_list)/n_animals)){
    for (j in 1:n_animals){
      track_list[[j+(i-1)*n_animals]][["id"]] <- as.factor(j)
      track_list[[j+(i-1)*n_animals]][["vid"]] <- as.factor(i)
    }
  }

  return(track_list)
}
