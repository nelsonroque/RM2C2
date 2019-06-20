#' RM2C2: Scoring, Summarizing

#' @name summary_mot
#' @export
summary_mot <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "MOT"
  summary.df <- df %>%
    group_by(.dots = c(group_var)) %>%
    summarise(mean.prop.correct.selections = mean(prop.correct.selections, na.rm=T),
              
              sum.correct.selections = sum(n.correct.selections, na.rm=T),
              sum.total.selections = sum(n.total.selections, na.rm=T),
              
              n.perfect.trials = sum(perfect_trial, na.rm=T),

              mean.selection_response_time = mean(selection_response_time, na.rm=T),
              median.selection_response_time = median(selection_response_time, na.rm=T),
              sd.selection_response_time = sd(selection_response_time, na.rm=T),
              
              n.trials = n(),
              
              mean.cue_before_time_deviance = mean(cue_before_time_deviance, na.rm=T),
              mean.cue_blink_time_deviance = mean(cue_blink_time_deviance, na.rm=T),
              mean.cue_show_time_deviance = mean(cue_show_time_deviance, na.rm=T),
              mean.cue_after_time_deviance = mean(cue_after_time_deviance, na.rm=T),
              
              mean.movement_time_deviance = mean(movement_time_deviance, na.rm=T)) %>%
    mutate(prop.perfect.trials = n.perfect.trials / n.trials) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
    
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}
