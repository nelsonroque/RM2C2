#' RM2C2: Scoring, Summarizing

#' @name score_color_shapes
#' @export
score_color_shapes <- function(df) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    mutate(HIT = ifelse(trial_type == 1 & button_pressed == 1, 1, 0),
           FA = ifelse(trial_type == 0 & button_pressed == 1, 1, 0),
           MISS = ifelse(trial_type == 1 & button_pressed == 0, 1, 0),
           CR = ifelse(trial_type == 0 & button_pressed == 0, 1, 0)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}