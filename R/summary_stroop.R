#' RM2C2: Scoring, Summarizing

#' @name summary_stroop
#' @export
summary_stroop <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
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
              
              median.RT.target_left_trials = median(RT[target_answer_location == "LEFT"], na.rm = T),
              mean.RT.target_left_trials = mean(RT[target_answer_location == "LEFT"], na.rm = T),
              sd.RT.target_left_trials = sd(RT[target_answer_location == "LEFT"], na.rm = T),
              
              median.RT.target_right_trials = median(RT[target_answer_location == "RIGHT"], na.rm = T),
              mean.RT.target_right_trials = mean(RT[target_answer_location == "RIGHT"], na.rm = T),
              sd.RT.target_right_trials = sd(RT[target_answer_location == "RIGHT"], na.rm = T),
              
              # accurate trials
              # //////////////////////////////////////////
              
              median.RT.congruent.accurate.trials = median(RT[trial_type == "C" & accuracy == 1], na.rm = T),
              mean.RT.congruent.accurate.trials = mean(RT[trial_type == "C" & accuracy == 1], na.rm = T),
              sd.RT.congruent.accurate.trials = sd(RT[trial_type == "C" & accuracy == 1], na.rm = T),
              
              median.RT.incongruent.accurate.trials = median(RT[trial_type == "I" & accuracy == 1], na.rm = T),
              mean.RT.incongruent.accurate.trials = mean(RT[trial_type == "I" & accuracy == 1], na.rm = T),
              sd.RT.incongruent.accurate.trials = sd(RT[trial_type == "I" & accuracy == 1], na.rm = T),
              
              median.RT.II.accurate.trials = median(RT[trial_sequence == "II" & accuracy == 1], na.rm = T),
              mean.RT.II.accurate.trials = mean(RT[trial_sequence == "II" & accuracy == 1], na.rm = T),
              sd.RT.II.accurate.trials = sd(RT[trial_sequence == "II" & accuracy == 1], na.rm = T),
              
              median.RT.CC.accurate.trials = median(RT[trial_sequence == "CC" & accuracy == 1], na.rm = T),
              mean.RT.CC.accurate.trials = mean(RT[trial_sequence == "CC" & accuracy == 1], na.rm = T),
              sd.RT.CC.accurate.trials = sd(RT[trial_sequence == "CC" & accuracy == 1], na.rm = T),
              
              median.RT.CI.accurate.trials = median(RT[trial_sequence == "CI" & accuracy == 1], na.rm = T),
              mean.RT.CI.accurate.trials = mean(RT[trial_sequence == "CI" & accuracy == 1], na.rm = T),
              sd.RT.CI.accurate.trials = sd(RT[trial_sequence == "CI" & accuracy == 1], na.rm = T),
              
              median.RT.IC.accurate.trials = median(RT[trial_sequence == "IC" & accuracy == 1], na.rm = T),
              mean.RT.IC.accurate.trials = mean(RT[trial_sequence == "IC" & accuracy == 1], na.rm = T),
              sd.RT.IC.accurate.trials = sd(RT[trial_sequence == "IC" & accuracy == 1], na.rm = T),
              
              # inaccurate trials
              # //////////////////////////////////////////
              
              median.RT.congruent.inaccurate.trials = median(RT[trial_type == "C" & accuracy == 0], na.rm = T),
              mean.RT.congruent.inaccurate.trials = mean(RT[trial_type == "C" & accuracy == 0], na.rm = T),
              sd.RT.congruent.inaccurate.trials = sd(RT[trial_type == "C" & accuracy == 0], na.rm = T),
              
              median.RT.incongruent.inaccurate.trials = median(RT[trial_type == "I" & accuracy == 0], na.rm = T),
              mean.RT.incongruent.inaccurate.trials = mean(RT[trial_type == "I" & accuracy == 0], na.rm = T),
              sd.RT.incongruent.inaccurate.trials = sd(RT[trial_type == "I" & accuracy == 0], na.rm = T),
              
              median.RT.II.inaccurate.trials = median(RT[trial_sequence == "II" & accuracy == 0], na.rm = T),
              mean.RT.II.inaccurate.trials = mean(RT[trial_sequence == "II" & accuracy == 0], na.rm = T),
              sd.RT.II.inaccurate.trials = sd(RT[trial_sequence == "II" & accuracy == 0], na.rm = T),
              
              median.RT.CC.inaccurate.trials = median(RT[trial_sequence == "CC" & accuracy == 0], na.rm = T),
              mean.RT.CC.inaccurate.trials = mean(RT[trial_sequence == "CC" & accuracy == 0], na.rm = T),
              sd.RT.CC.inaccurate.trials = sd(RT[trial_sequence == "CC" & accuracy == 0], na.rm = T),
              
              median.RT.CI.inaccurate.trials = median(RT[trial_sequence == "CI" & accuracy == 0], na.rm = T),
              mean.RT.CI.inaccurate.trials = mean(RT[trial_sequence == "CI" & accuracy == 0], na.rm = T),
              sd.RT.CI.inaccurate.trials = sd(RT[trial_sequence == "CI" & accuracy == 0], na.rm = T),
              
              median.RT.IC.inaccurate.trials = median(RT[trial_sequence == "IC" & accuracy == 0], na.rm = T),
              mean.RT.IC.inaccurate.trials = mean(RT[trial_sequence == "IC" & accuracy == 0], na.rm = T),
              sd.RT.IC.inaccurate.trials = sd(RT[trial_sequence == "IC" & accuracy == 0], na.rm = T),
              
              n.correct = sum(accuracy == 1),
              n.incorrect = sum(accuracy == 0),
              
              n.C = sum(trial_type == "C"),
              n.I = sum(trial_type == "I"),
              n.CC = sum(trial_sequence == "CC"),
              n.II = sum(trial_sequence == "II"),
              n.CI = sum(trial_sequence == "CI"),
              n.IC = sum(trial_sequence == "IC"),
              
              n.C.correct = sum(trial_type == "C" & accuracy == 1),
              n.I.correct = sum(trial_type == "I" & accuracy == 1),
              n.CC.correct = sum(trial_sequence == "CC" & accuracy == 1),
              n.II.correct = sum(trial_sequence == "II" & accuracy == 1),
              n.CI.correct = sum(trial_sequence == "CI" & accuracy == 1),
              n.IC.correct = sum(trial_sequence == "IC" & accuracy == 1),
              
              n = n()) %>%
    mutate(prop.correct = n.correct/n,
           prop.incorrect = n.incorrect/n,
           prop.C.correct = n.C.correct/n.C,
           prop.I.correct = n.I.correct/n.I,
           prop.CC.correct = n.CC.correct/n.CC,
           prop.II.correct = n.II.correct/n.II,
           prop.CI.correct = n.CI.correct/n.CI,
           prop.IC.correct = n.IC.correct/n.IC) %>%
    mutate(conflict.adaptation.effect.median = (median.RT.II.accurate.trials - median.RT.CI.accurate.trials) - (median.RT.CC.accurate.trials - median.RT.IC.accurate.trials),
           conflict.adaptation.effect.mean = (mean.RT.II.accurate.trials - mean.RT.CI.accurate.trials) - (mean.RT.CC.accurate.trials - mean.RT.IC.accurate.trials),
           conflict.adaptation.effect.sd = (sd.RT.II.accurate.trials - sd.RT.CI.accurate.trials) - (sd.RT.CC.accurate.trials - sd.RT.IC.accurate.trials)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}