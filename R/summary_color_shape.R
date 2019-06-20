#' RM2C2: Scoring, Summarizing

#' @name summary_color_shapes
#' @export
summary_color_shapes <- function(df,group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "COLOR_SHAPES"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time, na.rm=T),
              median.RT.HIT_trials = median(response_time[HIT == 1], na.rm=T),
              median.RT.FA_trials = median(response_time[FA == 1], na.rm=T),
              median.RT.MISS_trials = median(response_time[MISS == 1], na.rm=T),
              median.RT.CR_trials = median(response_time[CR == 1], na.rm=T),
              sd.RT.all_trials = sd(response_time, na.rm=T),
              sd.RT.HIT_trials = sd(response_time[HIT == 1], na.rm=T),
              sd.RT.FA_trials = sd(response_time[FA == 1], na.rm=T),
              sd.RT.MISS_trials = sd(response_time[MISS == 1], na.rm=T),
              sd.RT.CR_trials = sd(response_time[CR == 1], na.rm=T),
              n.HIT = sum(HIT),
              n.FA = sum(FA),
              n.MISS = sum(MISS),
              n.CR = sum(CR),
              n.change.trials = sum(trial_type == 1),
              n.no_change.trials = sum(trial_type == 0),
              n.filtered.trials = sum(is.na(response_time)),
              n.trials = n()) %>%
    mutate(HIT.rate = n.HIT/n.change.trials,
           FA.rate = n.FA/n.no_change.trials) %>%
    mutate(MISS.rate = 1 - HIT.rate,
           CR.rate = 1 - FA.rate) %>%
    SDT_adj(.) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}