#' RM2C2: Scoring, Summarizing

#' @name summary_visual_wm
#' @export
summary_visual_wm <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "VISUAL_WM"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time,na.rm=T),
              mean.RT.all_trials = mean(response_time,na.rm=T),
              sd.RT.all_trials = sd(response_time,na.rm=T),
              
              median.RT.multiple_choice_2opt = median(response_time[response_type == "MULTI_CHOICE_2"],na.rm=T),
              mean.RT.multiple_choice_2opt = mean(response_time[response_type == "MULTI_CHOICE_2"],na.rm=T),
              sd.RT.multiple_choice_2opt = sd(response_time[response_type == "MULTI_CHOICE_2"],na.rm=T),
              
              median.RT.multiple_choice_3opt = median(response_time[response_type == "MULTI_CHOICE_3"],na.rm=T),
              mean.RT.multiple_choice_3opt = mean(response_time[response_type == "MULTI_CHOICE_3"],na.rm=T),
              sd.RT.multiple_choice_3opt = sd(response_time[response_type == "MULTI_CHOICE_3"],na.rm=T),
              
              median.RT.free_rotate = median(response_time[response_type == "FREE_ROTATE"],na.rm=T),
              mean.RT.free_rotate = mean(response_time[response_type == "FREE_ROTATE"],na.rm=T),
              sd.RT.free_rotate = sd(response_time[response_type == "FREE_ROTATE"],na.rm=T),
              
              n.multiple_choice_correct = sum(mc_response_correct, na.rm=T),
              
              n.multiple_choice_trials = sum(response_type == "MULTI_CHOICE_2" | response_type == "MULTI_CHOICE_3"),
              n.multiple_choice_2opt_trials = sum(response_type == "MULTI_CHOICE_2"),
              n.multiple_choice_3opt_trials = sum(response_type == "MULTI_CHOICE_3"),
              n.free_rotate_trials = sum(response_type == "FREE_ROTATE"),
              n.trials = n()) %>%
    mutate(prop.multiple_choice.correct = n.multiple_choice_correct / n.multiple_choice_trials) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}

