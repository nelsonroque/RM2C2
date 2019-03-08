#' RM2C2: Scoring, Summarizing

#' @name summary_rspan
#' @export
summary_rspan <- function(df, group_var) {
  TASK_NAME <- "RSPAN"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.distractor = median(median.dist.RT, na.rm=T),
              mean.RT.distractor = mean(median.dist.RT, na.rm=T),
              sd.RT.distractor = sd(median.dist.RT, na.rm=T),
              median.RT.recall = median(recall.RT, na.rm=T),
              mean.RT.recall = mean(recall.RT, na.rm=T),
              sd.RT.recall = sd(recall.RT, na.rm=T),
              n.perfect.distractor = sum(perfect.distractor.trial),
              n.perfect.recall = sum(perfect.recall.trial),
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}
