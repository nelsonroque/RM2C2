#' RM2C2: Scoring, Summarizing

#' @name summary_seq_tapping
#' @export
summary_seq_tapping <- function(df, group_var, nontap_cols = c(1:24)) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "SEQUENCE_TAPPING"
  
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.acceleration = median(acceleration, na.rm = T),
              mean.acceleration = mean(acceleration, na.rm = T),
              sd.acceleration = sd(acceleration, na.rm = T),
              sd.abs.acceleration = sd(abs(acceleration), na.rm = T),
              max.acceleration = max(acceleration, na.rm = T),
              q90.acceleration = quantile(acceleration, .90, na.rm=T)[[1]][1],
              q95.acceleration = quantile(acceleration, .95, na.rm=T)[[1]][1],
              median.velocity = median(velocity, na.rm = T),
              mean.velocity = mean(velocity, na.rm = T),
              sd.velocity = sd(velocity, na.rm = T),
              max.velocity = max(velocity, na.rm = T),
              q90.velocity = quantile(velocity, .90, na.rm=T)[[1]][1],
              q95.velocity = quantile(velocity, .95, na.rm=T)[[1]][1],
              n.total.taps = n())
  
  summary2.df <- df %>%
    group_by_(.dots = group_var) %>%
    select(nontap_cols) %>%
    summarise(median.first_tap_time = median(homeTargetDelay, na.rm=T),
              mean.first_tap_time = median(homeTargetDelay, na.rm=T),
              sd.first_tap_time = sd(homeTargetDelay, na.rm=T),
              min.first_tap_time = min(homeTargetDelay, na.rm=T),
              max.first_tap_time = max(homeTargetDelay, na.rm=T),
              
              median.outbounds_taps = median(out_bounds_taps, na.rm=T),
              mean.outbounds_taps = mean(out_bounds_taps, na.rm=T),
              sum.outbounds_taps = sum(out_bounds_taps, na.rm=T),
              sd.outbounds_taps = sd(out_bounds_taps, na.rm=T),
              min.outbounds_taps = min(out_bounds_taps, na.rm=T),
              max.outbounds_taps = max(out_bounds_taps, na.rm=T),
              
              median.repeat_taps = median(repeat_taps, na.rm=T),
              mean.repeat_taps = mean(repeat_taps, na.rm=T),
              sum.repeat_taps = sum(repeat_taps, na.rm=T),
              sd.repeat_taps = sd(repeat_taps, na.rm=T),
              min.repeat_taps = min(repeat_taps, na.rm=T),
              max.repeat_taps = max(repeat_taps, na.rm=T),
              
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  names(summary2.df)[(len_group_var+1):ncol(summary2.df)] <- paste0(TASK_NAME,".",names(summary2.df)[(len_group_var+1):ncol(summary2.df)])
  
  final.df <- merge(summary.df,summary2.df) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  return(final.df)
}