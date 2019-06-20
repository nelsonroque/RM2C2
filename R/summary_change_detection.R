#' RM2C2: Scoring, Summarizing

#' @name summary_change_detection
#' @export
summary_change_detection <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME = "CHANGE_DETECTION"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time,na.rm=T),
              median.RT.HIT_trials = median(response_time[HIT.trial == 1],na.rm=T),
              median.RT.FA_trials = median(response_time[FA.trial == 1],na.rm=T),
              median.RT.MISS_trials = median(response_time[MISS.trial == 1],na.rm=T),
              median.RT.CR_trials = median(response_time[CR.trial == 1],na.rm=T),
              n.HIT = sum(HIT.trial),
              n.FA = sum(FA.trial),
              n.CR = sum(CR.trial),
              n.MISS = sum(MISS.trial),
              n.change.trials = sum(trial_type == "DIFFERENT"),
              n.nochange.trials = sum(trial_type == "SAME"),
              n.frames = n()) %>%
    mutate(HIT.rate = n.HIT/n.change.trials,
           FA.rate = n.FA/n.nochange.trials) %>%
    mutate(MISS.rate = 1 - HIT.rate,
           CR.rate = 1 - FA.rate) %>%
    SDT_adj(.) %>%
    mutate(k.score = CorRec.rate * square_num)  %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}