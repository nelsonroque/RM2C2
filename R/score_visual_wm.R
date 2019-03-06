#' @export
score_visual_wm <- function(df, threshold=15){
  scored <- df %>% 
    mutate(mc_response_correct = ifelse(user_response == target_response, 1, 0))
  return(scored)
}