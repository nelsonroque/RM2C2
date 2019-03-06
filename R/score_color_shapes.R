#' RM2C2: Scoring, Summarizing

#' @name score_color_shapes
score_color_shapes <- function(df) {
  scored <- df %>%
    mutate(HIT = ifelse(trial_type == 1 & button_pressed == 1, 1, 0),
           FA = ifelse(trial_type == 0 & button_pressed == 1, 1, 0),
           MISS = ifelse(trial_type == 1 & button_pressed == 0, 1, 0),
           CR = ifelse(trial_type == 0 & button_pressed == 0, 1, 0))
  return(scored)
}