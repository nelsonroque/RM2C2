#' RM2C2: Scoring, Summarizing

#' @name score_mot
#' @export
score_mot <- function(df) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    mutate(cue_before_time_deviance = t_cueBeforeTime - a_cueBeforeTime,
           cue_blink_time_deviance = t_cueBlinkTime - a_cueBlinkTime,
           cue_show_time_deviance = t_cueShowTime - a_cueShowTime,
           cue_after_time_deviance = t_cueAfterTime - a_cueAfterTime,
           movement_time_deviance = t_movementTime - a_movementTime,
           selection_response_time = totalResponseTime) %>%
    rowwise() %>%
    mutate(n.total.selections = length(sapply(strsplit(unlist(strsplit(selected_stim_list, " ")), "_"), `[`, 4)),
           n.correct.selections = sum(as.numeric(sapply(strsplit(unlist(strsplit(selected_stim_list, " ")), "_"), `[`, 4)))) %>%
    mutate(prop.correct.selections = n.correct.selections / n.total.selections) %>%
    mutate(perfect_trial = ifelse(target_num == n.correct.selections, 1, 0)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}
