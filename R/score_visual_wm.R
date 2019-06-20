#' RM2C2: Scoring, Summarizing

#' @name score_visual_wm
#' @export
score_visual_wm <- function(df, threshold=15){
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>% 
    mutate(mc_response_correct = ifelse(user_response == target_response, 1, 0)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}