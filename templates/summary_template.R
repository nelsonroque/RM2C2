#' RM2C2: Scoring, Summarizing

#' @name summary_
#' @export
summary_ <- function(df, group_var) {
  TASK_NAME <- "TASK_NAME"
  summary.df <- df %>%
    group_by(.dots = c(group_var)) %>%
    summarise(n.trials = n())
    
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}