#' RM2C2: Scoring, Summarizing

#' @name summary_symbol_search
#' @export
summary_symbol_search <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "SYMBOL_SEARCH"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    mutate(accuracy = ifelse(user_response == correct_response,1,0)) %>%
    summarise(median.RT.all_trials = median(response_time, na.rm=T),
              median.RT.accurate_trials = median(response_time[accuracy == 1], na.rm=T),
              median.RT.error_trials = median(response_time[accuracy == 0], na.rm=T),
              median.RT.lure_trials = median(response_time[trial_type == "LURE"], na.rm=T),
              median.RT.normal_trials = median(response_time[trial_type == "NORMAL"], na.rm=T),
              sd.RT.all_trials = sd(response_time, na.rm=T),
              sd.RT.accurate_trials = sd(response_time[accuracy == 1], na.rm=T),
              sd.RT.error_trials = sd(response_time[accuracy == 0], na.rm=T),
              sd.RT.lure_trials = sd(response_time[trial_type == "LURE"], na.rm=T),
              sd.RT.normal_trials = sd(response_time[trial_type == "NORMAL"], na.rm=T),
              proportion.accurate.trials = sum(accuracy)/n(),
              proportion.error.trials = (n() - sum(accuracy))/n(),
              n.filtered.trials = sum(is.na(response_time)),
              n.accurate.trials = sum(accuracy),
              n.error.trials = n() - sum(accuracy),
              n.lure.trials = sum(trial_type == "LURE"),
              n.normal.trials = sum(trial_type == "NORMAL"),
              # correct / incorrect tally for each of the breakdowns
              n.trials = n()) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}