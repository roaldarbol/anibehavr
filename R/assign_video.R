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

  n_vids <- track_list %>%
    bind_rows() %>%
    filter(time == 0) %>%
    distinct() %>%
    summarise(n_vids = n() / n_animals) %>%
    as_vector()

  for (i in 1:n_vids){
    for (j in 1:n_animals){
      track_list[[j+(i-1)*n_animals]][["id"]] <- as.factor(j)
      track_list[[j+(i-1)*n_animals]][["vid"]] <- as.factor(i)
    }
  }

  return(track_list)
}
