#' RM2C2: Scoring, Summarizing

#' @name summary_dot_memory_alt
#' @export
summary_dot_memory_alt <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "DOT_MEMORY"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time, na.rm=T),
              sd.RT.all_trials = sd(response_time, na.rm=T),
              
              median.RT.perfect_trials = median(response_time[is_perfect_trial == 1], na.rm=T),
              sd.RT.perfect_trials = sd(response_time[is_perfect_trial == 1], na.rm=T),
              
              median.RT.nonperfect_trials = median(response_time[is_perfect_trial == 0], na.rm=T),
              sd.RT.nonperfect_trials = sd(response_time[is_perfect_trial == 0], na.rm=T),
              
              median.error.distance.overall = median(sum_error_distance, na.rm=T),
              sd.error.distance.overall = sd(sum_error_distance, na.rm=T),
              sum.error.distance.overall = sum(sum_error_distance, na.rm=T),
              sum.ambiguous.responses = sum(n_ambiguous_responses, na.rm=T),
              mean.prop.ambiguous.responses = mean(prop_ambiguous_responses, na.rm=T),
              
              mean_hausdorff_distance = mean(hausdorff_distance, na.rm=T),
              min_hausdorff_distance = min(hausdorff_distance, na.rm=T),
              max_hausdorff_distance = max(hausdorff_distance, na.rm=T),
              sd_hausdorff_distance = sd(hausdorff_distance, na.rm=T),

              # no one is perfect, except those who get all dots perfectly!
              count.perfect.dots = sum(sum_perfect_dots),
              count.perfect.trials = sum(is_perfect_trial, na.rm=T),
              n.filtered.trials = sum(is.na(response_time)),
              n.trials = n()) %>%
    mutate(prop.perfect.trials = count.perfect.trials/n.trials) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names 
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}
