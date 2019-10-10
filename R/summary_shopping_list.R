#' RM2C2: Scoring, Summarizing

#' @name summary_shopping_list
#' @export
summary_shopping_list <- function(df, group_var, app_version="1_1_release_18_7_27") {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME = "SHOPPING_LIST"
  
  if(app_version != "1_1_release_18_7_27") {
    summary.df <- df %>%
      group_by_(.dots = group_var) %>%
      summarise(mean.RT.judgement = mean(judgement_RT, na.rm = T),
                median.RT.judgement = median(judgement_RT, na.rm = T),
                sd.RT.judgement = sd(judgement_RT, na.rm = T),
                mean.RT.choice = mean(choice_RT, na.rm = T),
                median.RT.choice = median(choice_RT, na.rm = T),
                sd.RT.choice = sd(choice_RT, na.rm = T),
                mean.RT.correct = mean(choice_RT[correct == 1], na.rm=T),
                median.RT.correct = median(choice_RT[correct == 1], na.rm=T),
                sd.RT.correct = sd(choice_RT[correct == 1], na.rm=T),
                mean.RT.incorrect = mean(choice_RT[correct == 0], na.rm=T),
                median.RT.incorrect = median(choice_RT[correct == 0], na.rm=T),
                sd.RT.incorrect = sd(choice_RT[correct == 0], na.rm=T),
                n.correct = sum(correct),
                n.incorrect = sum(correct == 0 & phase == 2),
                n = max(trial_num[phase == 2])) %>%
      mutate(prop.correct = n.correct/n,
             prop.incorrect = n.incorrect/n) %>%
      mutate(PACKAGE.VERSION = PACKAGE.VERSION)
    
  } else {
    summary.df <- df %>%
      group_by_(.dots = group_var) %>%
      summarise(mean.RT.judgement = mean(judgement_RT, na.rm = T),
                median.RT.judgement = median(judgement_RT, na.rm = T),
                sd.RT.judgement = sd(judgement_RT, na.rm = T),
                n.correct = sum(correct),
                n.incorrect = sum(correct == 0 & phase == 2),
                n = max(trial_num[phase == 2])) %>%
      mutate(prop.correct = n.correct/n,
             prop.incorrect = n.incorrect/n) %>%
      mutate(PACKAGE.VERSION = PACKAGE.VERSION)
    
  }


  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}