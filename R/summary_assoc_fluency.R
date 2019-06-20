#' RM2C2: Scoring, Summarizing

#' @name summary_assoc_fluency
#' @export
summary_assoc_fluency <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "ASSOCIATIONAL_FLUENCY"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(total_entries = sum(total_entries)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  

  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
  
  return(result)
}