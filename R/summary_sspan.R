#' RM2C2: Scoring, Summarizing

#' @name summary_sspan
#' @export
summary_sspan <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "SSPAN"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.distractor = median(median.dist.RT, na.rm=T),
              mean.RT.distractor = mean(median.dist.RT, na.rm=T),
              sd.RT.distractor = sd(median.dist.RT, na.rm=T),
              median.RT.recall = median(recall.RT, na.rm=T),
              mean.RT.recall = mean(recall.RT, na.rm=T),
              sd.RT.recall = sd(recall.RT, na.rm=T),
              sum.set_size = sum(set_size, na.rm=T),
              n.perfect.distractor = sum(perfect.distractor.trial),
              n.perfect.recall = sum(perfect.recall.trial),
              n.trials = n(),
              sum.FSM.score = sum(FSM.score, na.rm=T),
              sum.PSM.ordered.score = sum(PSM.ordered.score, na.rm=T),
              sum.PSM.unordered.score = sum(PSM.unordered.score, na.rm=T)) %>%
    mutate(prop.perfect.distractor.trials = n.perfect.distractor / n.trials,
           prop.perfect.recall.trials = n.perfect.recall / n.trials) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}