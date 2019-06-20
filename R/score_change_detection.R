#' RM2C2: Scoring, Summarizing

#' @name score_change_detection
#' @export
score_change_detection <- function(df) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    mutate(FA.trial = ifelse(trial_type == "DIFFERENT" & user_choice == "SAME",1,0),
           MISS.trial = ifelse(trial_type == "SAME" & user_choice == "DIFFERENT",1,0),
           HIT.trial = ifelse(trial_type == "SAME" & user_choice == "SAME",1,0),
           CR.trial = ifelse(trial_type == "DIFFERENT" & user_choice == "DIFFERENT",1,0)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}