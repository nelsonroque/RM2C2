#' RM2C2: Scoring, Summarizing

#' @name summary_color_speed
#' @export
summary_color_speed <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "COLOR_SPEED"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials_color = median(ColorRT, na.rm=T),
              mean.RT.all_trials_color = mean(ColorRT, na.rm=T),
              sd.RT.all_trials_color = sd(ColorRT, na.rm=T),
              
              median.RT.all_trials_location = median(LocRT, na.rm=T),
              mean.RT.all_trials_location = mean(LocRT, na.rm=T),
              sd.RT.all_trials_location = sd(LocRT, na.rm=T),
              
              
              n.color.accurate.trials = sum(color_accuracy),
              n.color.error.trials = n() - sum(color_accuracy),
              
              n.location.accurate.trials = n() - sum(is_location_response_outbounds),
              n.location.error.trials = sum(is_location_response_outbounds),
              
              n.trials = n()) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}