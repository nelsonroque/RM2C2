summary_stroop <- function(df, group_var) {
  TASK_NAME <- "STROOP"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(RT,na.rm=T),
              mean.RT.all_trials = mean(RT,na.rm=T),
              sd.RT.all_trials = sd(RT,na.rm=T),
              
              median.RT.correct_trials = median(RT[accuracy == 1], na.rm = T),
              mean.RT.correct_trials = mean(RT[accuracy == 1], na.rm = T),
              sd.RT.correct_trials = sd(RT[accuracy == 1], na.rm = T),
              
              median.RT.incorrect_trials = median(RT[accuracy == 0], na.rm = T),
              mean.RT.incorrect_trials = mean(RT[accuracy == 0], na.rm = T),
              sd.RT.incorrect_trials = sd(RT[accuracy == 0], na.rm = T),
              
              # without acc
              
              median.RT.target_left_trials = median(RT[target_answer_location == "LEFT"], na.rm = T),
              mean.RT.target_left_trials = mean(RT[target_answer_location == "LEFT"], na.rm = T),
              sd.RT.target_left_trials = sd(RT[target_answer_location == "LEFT"], na.rm = T),
              
              median.RT.target_right_trials = median(RT[target_answer_location == "RIGHT"], na.rm = T),
              mean.RT.target_right_trials = mean(RT[target_answer_location == "RIGHT"], na.rm = T),
              sd.RT.target_right_trials = sd(RT[target_answer_location == "RIGHT"], na.rm = T),
              
              median.RT.congruent_trials = median(RT[trial_type == "C" & accuracy == 1], na.rm = T),
              mean.RT.congruent_trials = mean(RT[trial_type == "C" & accuracy == 1], na.rm = T),
              sd.RT.congruent_trials = sd(RT[trial_type == "C" & accuracy == 1], na.rm = T),
              
              median.RT.incongruent_trials = median(RT[trial_type == "I" & accuracy == 1], na.rm = T),
              mean.RT.incongruent_trials = mean(RT[trial_type == "I" & accuracy == 1], na.rm = T),
              sd.RT.incongruent_trials = sd(RT[trial_type == "I" & accuracy == 1], na.rm = T),
              
              
              median.RT.II_trials = median(RT[trial_sequence == "II" & accuracy == 1], na.rm = T),
              mean.RT.II_trials = mean(RT[trial_sequence == "II" & accuracy == 1], na.rm = T),
              sd.RT.II_trials = sd(RT[trial_sequence == "II" & accuracy == 1], na.rm = T),
              
              median.RT.CC_trials = median(RT[trial_sequence == "CC" & accuracy == 1], na.rm = T),
              mean.RT.CC_trials = mean(RT[trial_sequence == "CC" & accuracy == 1], na.rm = T),
              sd.RT.CC_trials = sd(RT[trial_sequence == "CC" & accuracy == 1], na.rm = T),
              
              median.RT.CI_trials = median(RT[trial_sequence == "CI" & accuracy == 1], na.rm = T),
              mean.RT.CI_trials = mean(RT[trial_sequence == "CI" & accuracy == 1], na.rm = T),
              sd.RT.CI_trials = sd(RT[trial_sequence == "CI" & accuracy == 1], na.rm = T),
              
              median.RT.IC_trials = median(RT[trial_sequence == "IC" & accuracy == 1], na.rm = T),
              mean.RT.IC_trials = mean(RT[trial_sequence == "IC" & accuracy == 1], na.rm = T),
              sd.RT.IC_trials = sd(RT[trial_sequence == "IC" & accuracy == 1], na.rm = T),
              
              n.correct = sum(accuracy == 1),
              n.incorrect = sum(accuracy == 0),
              
              n.C = sum(trial_sequence == "C"),
              n.I = sum(trial_sequence == "I"),
              
              n.CC = sum(trial_sequence == "CC"),
              n.II = sum(trial_sequence == "II"),
              n.CI = sum(trial_sequence == "CI"),
              n.IC = sum(trial_sequence == "IC"),
              
              n = n()) %>%
    mutate(prop.correct = n.correct/n,
           prop.incorrect = n.incorrect/n) %>%
    mutate(conflict.adaptation.effect.median = (median.RT.II_trials - median.RT.CI_trials) - (median.RT.CC_trials - median.RT.IC_trials),
           conflict.adaptation.effect.mean = (mean.RT.II_trials - mean.RT.CI_trials) - (mean.RT.CC_trials - mean.RT.IC_trials),
           conflict.adaptation.effect.sd = (sd.RT.II_trials - sd.RT.CI_trials) - (sd.RT.CC_trials - sd.RT.IC_trials))
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}