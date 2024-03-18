#' Assign video
#'
#' @param track_list List from video files
#' @param animal_ids Animal ids
#'
#' @return track_list
#' @export

assign_video <- function(track_list, animal_ids, tracker = c("trex", "idtrackerai")) {
  n_animals <- animal_ids %>%
    na.omit() %>%
    length()

  if (tracker == "trex"){
    n_vids <- length(track_list) / n_animals

    for (i in 1:n_vids){
      for (j in 1:n_animals){
        track_list[[j+(i-1)*n_animals]][["id"]] <- as.factor(j)
        track_list[[j+(i-1)*n_animals]][["vid"]] <- as.factor(i)
      }
    }
  } else if (tracker == "idtrackerai"){
    n_vids <- length(track_list)
    for (i in 1:n_vids){
      track_list[[i]][["vid"]] <- as.factor(i)
    }
  }

  return(track_list)
}
