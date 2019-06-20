#' RM2C2: Scoring, Summarizing

#' @name summary_dot_memory_interference
#' @export
summary_dot_memory_interference <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "DOT_MEMORY_INTERFERENCE"
  
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(n.correct.taps = sum(tap_correct[tap_correct == 1]),
              n.total.taps = n()) %>%
    mutate(n.incorrect.taps = n.total.taps - n.correct.taps) %>%
    select(user_id, n.total.taps, n.correct.taps, n.incorrect.taps) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)

    #print("ERROR: Please use `restructure_interference_data()` first to restructure Dot Memory interference data")
}