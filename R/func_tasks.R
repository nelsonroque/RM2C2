# ===================================================================================
# SYMBOL SEARCH
# ===================================================================================

# for scoring raw, parsed data

# for summarizing scored data
summary_symbol_search <- function(df, group_var) {
  TASK_NAME <- "SYMBOL_SEARCH"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    mutate(accuracy = ifelse(user_response == correct_response,1,0)) %>%
    summarise(median.RT.all_trials = median(response_time, na.rm=T),
              median.RT.accurate_trials = median(response_time[accuracy == 1], na.rm=T),
              median.RT.error_trials = median(response_time[accuracy == 0], na.rm=T),
              median.RT.lure_trials = median(response_time[trial_type == "LURE"], na.rm=T),
              median.RT.normal_trials = median(response_time[trial_type == "NORMAL"], na.rm=T),
              sd.RT.all_trials = sd(response_time, na.rm=T),
              sd.RT.accurate_trials = sd(response_time[accuracy == 1], na.rm=T),
              sd.RT.error_trials = sd(response_time[accuracy == 0], na.rm=T),
              sd.RT.lure_trials = sd(response_time[trial_type == "LURE"], na.rm=T),
              sd.RT.normal_trials = sd(response_time[trial_type == "NORMAL"], na.rm=T),
              proportion.accurate.trials = sum(accuracy)/n(),
              proportion.error.trials = (n() - sum(accuracy))/n(),
              n.filtered.trials = sum(is.na(response_time)),
              n.accurate.trials = sum(accuracy),
              n.error.trials = n() - sum(accuracy),
              n.lure.trials = sum(trial_type == "LURE"),
              n.normal.trials = sum(trial_type == "NORMAL"),
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}

# ===================================================================================
# COLOR SPEED
# ===================================================================================

# for scoring raw, parsed data
score_color_speed <- function(df, threshold=75) {
  scored <- df %>% 
    mutate(color_accuracy = ifelse(color == ColorChoice, 1, 0)) %>%
    separate(Loc1, into=c("cue_x", "cue_y"), " ", convert=T) %>%
    separate(TouchLocation, into=c("touch_x", "touch_y"), " ", convert=T) %>%
    mutate(location.precision = distance(as.numeric(touch_x), as.numeric(cue_x), as.numeric(touch_y), as.numeric(cue_y))) %>%
    mutate(is_location_response_outbounds = ifelse(location.precision > threshold, 1, 0))
  return(scored)
}

# for summarizing scored data
summary_color_speed <- function(df, group_var) {
  TASK_NAME <- "COLOR_SPEED"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials_color = median(ColorRT, na.rm=T),
              mean.RT.all_trials_color = mean(ColorRT, na.rm=T),
              sd.RT.all_trials_color = sd(ColorRT, na.rm=T),
              
              median.RT.all_trials_location = median(LocRT, na.rm=T),
              mean.RT.all_trials_location = mean(LocRT, na.rm=T),
              sd.RT.all_trials_location = sd(LocRT, na.rm=T),
              
              
              n.color.accurate.trials = sum(color_accuracy),
              n.color.error.trials = n() - sum(color_accuracy),
              
              n.location.accurate.trials = n() - sum(is_location_response_outbounds),
              n.location.error.trials = sum(is_location_response_outbounds),
              
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}

# ===================================================================================
# DOT MEMORY
# ===================================================================================

# for scoring raw, parsed data
score_dot_memory <- function(df, square_size=5) {
  scored <- df %>% 
    separate(dot_locations, c("dot1","dot2","dot3"), " ", convert=T) %>%
    separate(dot1, c("dot1_rx", "dot1_ry"), "_", convert=T) %>%
    separate(dot2, c("dot2_rx", "dot2_ry"), "_", convert=T) %>%
    separate(dot3, c("dot3_rx", "dot3_ry"), "_", convert=T) %>%
    separate(user_answers, c('user_dot1', "user_dot2", "user_dot3"), " ", convert=T) %>%
    separate(user_dot1, c("user_dot1_rx", "user_dot1_ry"), "_", convert=T) %>%
    separate(user_dot2, c("user_dot2_rx", "user_dot2_ry"), "_", convert=T) %>%
    separate(user_dot3, c("user_dot3_rx", "user_dot3_ry"), "_", convert=T) %>%
    mutate_at(.vars = vars(dot1_rx:user_dot3_ry),
              .funs = funs(`1`=add_to(.,1))) %>%
    mutate_at(.vars = vars(dot1_rx_1:user_dot3_ry_1),
              .funs = funs(`coord`=mult_by(.,square_size))) %>%
    mutate(r1_distance = distance(user_dot1_rx_1_coord, dot1_rx_1_coord,
                                  user_dot1_ry_1_coord, dot1_ry_1_coord),
           r2_distance = distance(user_dot2_rx_1_coord, dot2_rx_1_coord,
                                  user_dot2_ry_1_coord, dot2_ry_1_coord),
           r3_distance = distance(user_dot3_rx_1_coord, dot3_rx_1_coord,
                                  user_dot3_ry_1_coord, dot3_ry_1_coord)) %>%
    mutate(r1_perfect = ifelse(r1_distance == 0, 1, 0),
           r2_perfect = ifelse(r2_distance == 0, 1, 0),
           r3_perfect = ifelse(r3_distance == 0, 1, 0),
           perfect_response = ifelse(r1_distance == 0 & r2_distance == 0 & r3_distance == 0,1,0)) %>%
    rowwise() %>%
    mutate(sum_perfect_dots = sum(c(r1_perfect, r2_perfect, r3_perfect)),
           median_error_distance = median(c(r1_distance, r2_distance, r3_distance)),
           sum_error_distance = sum(c(r1_distance, r2_distance, r3_distance)))
    
  return(scored)
}

# for summarizing scored data
summary_dot_memory <- function(df, group_var) {
  TASK_NAME <- "DOT_MEMORY"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time, na.rm=T),
              sd.RT.all_trials = sd(response_time, na.rm=T),
              
              median.RT.perfect_trials = median(response_time[perfect_response == 1], na.rm=T),
              sd.RT.perfect_trials = sd(response_time[perfect_response == 1], na.rm=T),
              
              median.RT.nonperfect_trials = median(response_time[perfect_response == 0], na.rm=T),
              sd.RT.nonperfect_trials = sd(response_time[perfect_response == 0], na.rm=T),
              
              median.error.distance.overall = median(sum_error_distance, na.rm=T),
              sd.error.distance.overall = sd(sum_error_distance, na.rm=T),
              sum.error.distance.overall = sum(sum_error_distance, na.rm=T),

              # no one is perfect, except those who get all dots perfectly!
              count.perfect.dots = sum(sum_perfect_dots),
              count.perfect.trials = sum(perfect_response, na.rm=T),
              n.filtered.trials = sum(is.na(response_time)),
              n.trials = n()) %>%
    mutate(prop.perfect.trials = count.perfect.trials/n.trials)
  
  # add task name to column names 
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}

# ===================================================================================
# COLOR SHAPES
# ===================================================================================

# for scoring raw, parsed data
score_color_shapes <- function(df) {
  scored <- df %>%
    mutate(HIT = ifelse(trial_type == 1 & button_pressed == 1, 1, 0),
           FA = ifelse(trial_type == 0 & button_pressed == 1, 1, 0),
           MISS = ifelse(trial_type == 1 & button_pressed == 0, 1, 0),
           CR = ifelse(trial_type == 0 & button_pressed == 0, 1, 0))
  return(scored)
}

# for summarizing scored data
summary_color_shapes <- function(df,group_var) {
  TASK_NAME <- "COLOR_SHAPES"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time, na.rm=T),
              median.RT.HIT_trials = median(response_time[HIT == 1], na.rm=T),
              median.RT.FA_trials = median(response_time[FA == 1], na.rm=T),
              median.RT.MISS_trials = median(response_time[MISS == 1], na.rm=T),
              median.RT.CR_trials = median(response_time[CR == 1], na.rm=T),
              sd.RT.all_trials = sd(response_time, na.rm=T),
              sd.RT.HIT_trials = sd(response_time[HIT == 1], na.rm=T),
              sd.RT.FA_trials = sd(response_time[FA == 1], na.rm=T),
              sd.RT.MISS_trials = sd(response_time[MISS == 1], na.rm=T),
              sd.RT.CR_trials = sd(response_time[CR == 1], na.rm=T),
              n.HIT = sum(HIT),
              n.FA = sum(FA),
              n.MISS = sum(MISS),
              n.CR = sum(CR),
              n.change.trials = sum(trial_type == 1),
              n.no_change.trials = sum(trial_type == 0),
              n.filtered.trials = sum(is.na(response_time)),
              n.trials = n()) %>%
    mutate(HIT.rate = n.HIT/n.change.trials,
           FA.rate = n.FA/n.no_change.trials) %>%
    mutate(MISS.rate = 1 - HIT.rate,
           CR.rate = 1 - FA.rate) %>%
    SDT_adj(.)
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}

# ===================================================================================
# GO NOGO
# ===================================================================================

# for scoring raw, parsed data
score_go_nogo <- function(df, nogo_letter = "X") {
  scored <- df %>%
    mutate(no.go.letter = nogo_letter,
           response_time = ifelse(responseTime == -1,NA,responseTime)) %>%
    mutate(is.nogo.frame = ifelse(letter == no.go.letter, 1, 0),
           FA = ifelse(is.nogo.frame == 1 & response == 1,1,0),
           MISS = ifelse(is.nogo.frame == 0 & response == 0,1,0),
           HIT = ifelse(is.nogo.frame == 0 & response == 1,1,0),
           CR = ifelse(is.nogo.frame == 1 & response == 0,1,0)) 
  return(scored)
}

# commission, 

# for summarizing scored data
summary_go_nogo <- function(df, group_var) {
  TASK_NAME <- "GO_NOGO"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time,na.rm=T),
              median.RT.HIT_trials = median(response_time[HIT == 1],na.rm=T),
              median.RT.FA_trials = median(response_time[FA == 1],na.rm=T),
              median.RT.MISS_trials = median(response_time[MISS == 1],na.rm=T),
              median.RT.CR_trials = median(response_time[CR == 1],na.rm=T),
              n.HIT = sum(HIT),
              n.FA = sum(FA),
              n.CR = sum(CR),
              n.MISS = sum(MISS),
              n.nogo.frames = sum(is.nogo.frame),
              n.frames = n()) %>%
    mutate(n.go.frames = n.frames - n.nogo.frames) %>%
    mutate(HIT.rate = n.HIT/n.go.frames,
           FA.rate = n.FA/n.nogo.frames) %>%
    mutate(MISS.rate = 1 - HIT.rate,
           CR.rate = 1 - FA.rate) %>%
    SDT_adj(.)
  
    # add task name to column names
    len_group_var = length(group_var)
    names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
    return(summary.df)
}

# ===================================================================================
# CHANGE DETECTION
# ===================================================================================

# for scoring raw, parsed data
score_change_detection <- function(df) {
  scored <- df %>%
    mutate(FA.trial = ifelse(trial_type == "DIFFERENT" & user_choice == "SAME",1,0),
           MISS.trial = ifelse(trial_type == "SAME" & user_choice == "DIFFERENT",1,0),
           HIT.trial = ifelse(trial_type == "SAME" & user_choice == "SAME",1,0),
           CR.trial = ifelse(trial_type == "DIFFERENT" & user_choice == "DIFFERENT",1,0)) 
  return(scored)
}

# for summarizing scored data
summary_change_detection <- function(df, group_var) {
  TASK_NAME = "CHANGE_DETECTION"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time,na.rm=T),
              median.RT.HIT_trials = median(response_time[HIT.trial == 1],na.rm=T),
              median.RT.FA_trials = median(response_time[FA.trial == 1],na.rm=T),
              median.RT.MISS_trials = median(response_time[MISS.trial == 1],na.rm=T),
              median.RT.CR_trials = median(response_time[CR.trial == 1],na.rm=T),
              n.HIT = sum(HIT.trial),
              n.FA = sum(FA.trial),
              n.CR = sum(CR.trial),
              n.MISS = sum(MISS.trial),
              n.change.trials = sum(trial_type == "DIFFERENT"),
              n.nochange.trials = sum(trial_type == "SAME"),
              n.frames = n()) %>%
    mutate(HIT.rate = n.HIT/n.change.trials,
           FA.rate = n.FA/n.nochange.trials) %>%
    mutate(MISS.rate = 1 - HIT.rate,
           CR.rate = 1 - FA.rate) %>%
    SDT_adj(.)
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}


# ===================================================================================
# SHOPPING LIST
# ===================================================================================

# for scoring raw, parsed data
score_shopping_list <- function(df){
  scored <- shopping_list %>%
    mutate(correct = ifelse(target_price == choice, 1, 0),
           judgement_RT = ifelse(judgement_RT == ".", NA, as.numeric(judgement_RT)),
           choice_RT = ifelse(choiceRT == ".", NA, as.numeric(choiceRT)),
           session_id = session_id[1])
  return(scored)
}

# for summarizing scored data
summary_shopping_list <- function(df, group_var) {
  TASK_NAME = "SHOPPING_LIST"
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
            n = max(trial_num)) %>%
    mutate(prop.correct = n.correct/n,
           prop.incorrect = n.incorrect/n)

  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}

# ===================================================================================
# STROOP
# ===================================================================================

# for scoring raw, parsed data
score_stroop <- function(df) {
  scored <- df
  return(scored)
}

# for summarizing scored data
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

# ===================================================================================
# OPERATION SPAN
# ===================================================================================

# for scoring raw, parsed data
score_ospan <- function(df, threshold=0.6){
  scored <- df %>%
    mutate(dist1.RESP = ifelse(dist1.RESP == ".", NA, dist1.RESP),
           dist2.RESP = ifelse(dist2.RESP == ".", NA, dist2.RESP),
           dist3.RESP = ifelse(dist3.RESP == ".", NA, dist3.RESP),
           dist4.RESP = ifelse(dist4.RESP == ".", NA, dist4.RESP),
           dist1.CRESP = ifelse(dist1.CRESP == ".", NA, dist1.CRESP),
           dist2.CRESP = ifelse(dist2.CRESP == ".", NA, dist2.CRESP),
           dist3.CRESP = ifelse(dist3.CRESP == ".", NA, dist3.CRESP),
           dist4.CRESP = ifelse(dist4.CRESP == ".", NA, dist4.CRESP),
           mem1 = ifelse(mem1 == ".", NA, mem1),
           mem2 = ifelse(mem2 == ".", NA, mem2),
           mem3 = ifelse(mem3 == ".", NA, mem3),
           mem4 = ifelse(mem4 == ".", NA, mem4)) %>%
    mutate(dist1.is.correct = ifelse(dist1.RESP == dist1.CRESP, 1, 0),
           dist2.is.correct = ifelse(dist2.RESP == dist2.CRESP, 1, 0),
           dist3.is.correct = ifelse(dist3.RESP == dist3.CRESP, 1, 0),
           dist4.is.correct = ifelse(dist4.RESP == dist4.CRESP, 1, 0),
           mem1.is.correct = ifelse(mem1 == Choice1, 1, 0),
           mem2.is.correct = ifelse(mem2 == Choice2, 1, 0),
           mem3.is.correct = ifelse(mem3 == Choice3, 1, 0),
           mem4.is.correct = ifelse(mem4 == Choice4, 1, 0)) %>%
    rowwise() %>%
    mutate(sum.dist.correct = sum(c(dist1.is.correct,dist2.is.correct,dist3.is.correct,dist4.is.correct),na.rm=T),
           sum.mem.correct = sum(c(mem1.is.correct, mem2.is.correct, mem3.is.correct, mem4.is.correct),na.rm=T),
           sum.dist.RT = sum(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           mean.dist.RT = mean(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           median.dist.RT = median(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           sd.dist.RT = sd(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT), na.rm=T),
           IQR.dist.RT = IQR(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT), na.rm=T)) %>%
    mutate(perfect.distractor = ifelse(sum.dist.correct == set_size, 1, 0),
           perfect.recall = ifelse(sum.mem.correct == set_size, 1, 0)) %>%
    mutate(prop.dist.accuracy = sum.dist.correct / set_size,
           prop.mem.accuracy = sum.mem.correct / set_size) %>%
    mutate(flag.dist.low.accuracy = ifelse(prop.dist.accuracy < threshold, 1, 0),
           flag.mem.low.accuracy = ifelse(prop.mem.accuracy < threshold, 1, 0)) %>%
    span_order(.)
  
  return(scored)
}

# for summarizing scored data
summary_ospan <- function(df, group_var) {
  TASK_NAME <- "OSPAN"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.distractor = median(median.dist.RT, na.rm=T),
              mean.RT.distractor = mean(median.dist.RT, na.rm=T),
              sd.RT.distractor = sd(median.dist.RT, na.rm=T),
              median.RT.recall = median(recall.RT, na.rm=T),
              mean.RT.recall = mean(recall.RT, na.rm=T),
              sd.RT.recall = sd(recall.RT, na.rm=T),
              mean.prop.dist.accuracy = mean(prop.dist.accuracy),
              sd.prop.dist.accuracy = sd(prop.dist.accuracy),
              mean.prop.mem.accuracy = mean(prop.mem.accuracy),
              sd.prop.mem.accuracy = sd(prop.mem.accuracy),
              n.perfect.distractor = sum(perfect.distractor),
              n.perfect.recall = sum(perfect.recall),
              n.flagged.distractor = sum(flag.dist.low.accuracy == 1),
              n.flagged.recall = sum(flag.mem.low.accuracy == 1),
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}


# ===================================================================================
# SYMMETRY SPAN
# ===================================================================================

# for scoring raw, parsed data
score_sspan <- function(df, threshold=0.6){
  scored <- df %>%
    mutate(dist1.RESP = ifelse(dist1.RESP == ".", NA, dist1.RESP),
           dist2.RESP = ifelse(dist2.RESP == ".", NA, dist2.RESP),
           dist3.RESP = ifelse(dist3.RESP == ".", NA, dist3.RESP),
           dist4.RESP = ifelse(dist4.RESP == ".", NA, dist4.RESP),
           dist1.CRESP = ifelse(dist1.CRESP == ".", NA, dist1.CRESP),
           dist2.CRESP = ifelse(dist2.CRESP == ".", NA, dist2.CRESP),
           dist3.CRESP = ifelse(dist3.CRESP == ".", NA, dist3.CRESP),
           dist4.CRESP = ifelse(dist4.CRESP == ".", NA, dist4.CRESP),
           mem1 = ifelse(mem1 == ".", NA, mem1),
           mem2 = ifelse(mem2 == ".", NA, mem2),
           mem3 = ifelse(mem3 == ".", NA, mem3),
           mem4 = ifelse(mem4 == ".", NA, mem4)) %>%
    mutate(dist1.is.correct = ifelse(dist1.RESP == dist1.CRESP, 1, 0),
           dist2.is.correct = ifelse(dist2.RESP == dist2.CRESP, 1, 0),
           dist3.is.correct = ifelse(dist3.RESP == dist3.CRESP, 1, 0),
           dist4.is.correct = ifelse(dist4.RESP == dist4.CRESP, 1, 0),
           mem1.is.correct = ifelse(mem1 == Choice1, 1, 0),
           mem2.is.correct = ifelse(mem2 == Choice2, 1, 0),
           mem3.is.correct = ifelse(mem3 == Choice3, 1, 0),
           mem4.is.correct = ifelse(mem4 == Choice4, 1, 0)) %>%
    rowwise() %>%
    mutate(sum.dist.correct = sum(c(dist1.is.correct,dist2.is.correct,dist3.is.correct,dist4.is.correct),na.rm=T),
           sum.mem.correct = sum(c(mem1.is.correct, mem2.is.correct, mem3.is.correct, mem4.is.correct),na.rm=T),
           sum.dist.RT = sum(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           mean.dist.RT = mean(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           median.dist.RT = median(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           sd.dist.RT = sd(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT), na.rm=T),
           IQR.dist.RT = IQR(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT), na.rm=T)) %>%
    mutate(perfect.distractor = ifelse(sum.dist.correct == set_size, 1, 0),
           perfect.recall = ifelse(sum.mem.correct == set_size, 1, 0)) %>%
    mutate(prop.dist.accuracy = sum.dist.correct / set_size,
           prop.mem.accuracy = sum.mem.correct / set_size) %>%
    mutate(flag.dist.low.accuracy = ifelse(prop.dist.accuracy < threshold, 1, 0),
           flag.mem.low.accuracy = ifelse(prop.mem.accuracy < threshold, 1, 0)) %>%
    span_order(.)
  
  return(scored)
}

# for summarizing scored data
summary_sspan <- function(df, group_var) {
  TASK_NAME <- "SSPAN"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.distractor = median(median.dist.RT, na.rm=T),
              mean.RT.distractor = mean(median.dist.RT, na.rm=T),
              sd.RT.distractor = sd(median.dist.RT, na.rm=T),
              median.RT.recall = median(recall.RT, na.rm=T),
              mean.RT.recall = mean(recall.RT, na.rm=T),
              sd.RT.recall = sd(recall.RT, na.rm=T),
              mean.prop.dist.accuracy = mean(prop.dist.accuracy),
              sd.prop.dist.accuracy = sd(prop.dist.accuracy),
              mean.prop.mem.accuracy = mean(prop.mem.accuracy),
              sd.prop.mem.accuracy = sd(prop.mem.accuracy),
              n.perfect.distractor = sum(perfect.distractor),
              n.perfect.recall = sum(perfect.recall),
              n.flagged.distractor = sum(flag.dist.low.accuracy == 1),
              n.flagged.recall = sum(flag.mem.low.accuracy == 1),
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}


# ===================================================================================
# ROTATION SPAN
# ===================================================================================

# for scoring raw, parsed data
score_rspan <- function(df, threshold=0.6){
  scored <- df %>%
    mutate(dist1.RESP = ifelse(dist1.RESP == ".", NA, dist1.RESP),
           dist2.RESP = ifelse(dist2.RESP == ".", NA, dist2.RESP),
           dist3.RESP = ifelse(dist3.RESP == ".", NA, dist3.RESP),
           dist4.RESP = ifelse(dist4.RESP == ".", NA, dist4.RESP),
           dist5.RESP = ifelse(dist5.RESP == ".", NA, dist5.RESP),
           dist1.CRESP = ifelse(dist1.CRESP == ".", NA, dist1.CRESP),
           dist2.CRESP = ifelse(dist2.CRESP == ".", NA, dist2.CRESP),
           dist3.CRESP = ifelse(dist3.CRESP == ".", NA, dist3.CRESP),
           dist4.CRESP = ifelse(dist4.CRESP == ".", NA, dist4.CRESP),
           dist5.CRESP = ifelse(dist5.CRESP == ".", NA, dist5.CRESP),
           mem1 = ifelse(mem1 == ".", NA, mem1),
           mem2 = ifelse(mem2 == ".", NA, mem2),
           mem3 = ifelse(mem3 == ".", NA, mem3),
           mem4 = ifelse(mem4 == ".", NA, mem4),
           mem5 = ifelse(mem5 == ".", NA, mem5)) %>%
    mutate(dist1.is.correct = ifelse(dist1.RESP == dist1.CRESP, 1, 0),
           dist2.is.correct = ifelse(dist2.RESP == dist2.CRESP, 1, 0),
           dist3.is.correct = ifelse(dist3.RESP == dist3.CRESP, 1, 0),
           dist4.is.correct = ifelse(dist4.RESP == dist4.CRESP, 1, 0),
           dist5.is.correct = ifelse(dist5.RESP == dist5.CRESP, 1, 0),
           mem1.is.correct = ifelse(mem1 == Choice1, 1, 0),
           mem2.is.correct = ifelse(mem2 == Choice2, 1, 0),
           mem3.is.correct = ifelse(mem3 == Choice3, 1, 0),
           mem4.is.correct = ifelse(mem4 == Choice4, 1, 0),
           mem5.is.correct = ifelse(mem5 == Choice5, 1, 0)) %>%
    rowwise() %>%
    mutate(sum.dist.correct = sum(c(dist1.is.correct, dist2.is.correct, dist3.is.correct, dist4.is.correct, dist5.is.correct),na.rm=T),
           sum.mem.correct = sum(c(mem1.is.correct, mem2.is.correct, mem3.is.correct, mem4.is.correct, mem5.is.correct),na.rm=T),
           sum.dist.RT = sum(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT), as.numeric(dist5.RT)), na.rm=T),
           mean.dist.RT = mean(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT), as.numeric(dist5.RT)), na.rm=T),
           median.dist.RT = median(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT), as.numeric(dist5.RT)), na.rm=T),
           sd.dist.RT = sd(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT, dist5.RT), na.rm=T),
           IQR.dist.RT = IQR(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT, dist5.RT), na.rm=T)) %>%
    mutate(perfect.distractor = ifelse(sum.dist.correct == set_size, 1, 0),
           perfect.recall = ifelse(sum.mem.correct == set_size, 1, 0)) %>%
    mutate(prop.dist.accuracy = sum.dist.correct / set_size,
           prop.mem.accuracy = sum.mem.correct / set_size) %>%
    mutate(flag.dist.low.accuracy = ifelse(prop.dist.accuracy < threshold, 1, 0),
           flag.mem.low.accuracy = ifelse(prop.mem.accuracy < threshold, 1, 0)) %>%
    span_order(.)
  
  return(scored)
}

# for summarizing scored data
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
              mean.prop.dist.accuracy = mean(prop.dist.accuracy),
              sd.prop.dist.accuracy = sd(prop.dist.accuracy),
              mean.prop.mem.accuracy = mean(prop.mem.accuracy),
              sd.prop.mem.accuracy = sd(prop.mem.accuracy),
              n.perfect.distractor = sum(perfect.distractor),
              n.perfect.recall = sum(perfect.recall),
              n.flagged.distractor = sum(flag.dist.low.accuracy == 1),
              n.flagged.recall = sum(flag.mem.low.accuracy == 1),
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}


# ===================================================================================
# QUICK TAPPING
# ===================================================================================

# for restructuring raw, parsed data
restructure_tapping_data <- function(df, nontap_cols = c(1:17), tap_col = c(18)) {
  restructured <- data.frame()
  for(row in 1:nrow(df)){
    row.values.nontap <- df[row,nontap_cols]
    row.values.tap <- df[row,tap_col]
    vals <- strsplit(unlist(strsplit(row.values.tap$tap_values,"\\+")),"_")
    vals.df <- data.frame(t(sapply(vals,c)))
    names(vals.df) <- c("tap_x","tap_y","a", "b", "c", "d")
    vals.df$tap_number <- seq(1,nrow(vals.df),1)
    export.row <- cbind(row.values.nontap,vals.df)
    restructured <- rbind(restructured,export.row)
  }
  
  restructured <- restructured %>%
    mutate(tap_x = as.numeric(as.character(tap_x)),
           tap_y = as.numeric(as.character(tap_y)))
  
  return(restructured)
}

# for scoring parsed restructured data
score_quick_tapping <- function(df, t_lag=3){
  scored <- df %>%
    separate(center_target_left, into=c("targ_left_x", "targ_left_y"), " ", convert=T) %>%
    separate(center_target_right, into=c("targ_right_x", "targ_right_y"), " ",convert=T) %>%
    mutate(tap_distance_left = distance(targ_left_x, tap_x, targ_left_y, tap_x),
           tap_distance_right = distance(targ_right_x, tap_x, targ_right_y, tap_x)) %>%
    mutate(tapped_circle = recode(b, `0` = 'LEFT', `1` = "RIGHT", `-1`  = "NONE"),
           target_circle = recode(c, `0` = 'LEFT', `1` = "RIGHT", `-1`  = "NONE")) %>%
    mutate(correct_circle_tap = ifelse(as.character(tapped_circle) == as.character(target_circle), 1, 0)) %>%
    group_by(user_id,game_uuid,trial_num) %>%
    mutate(velocity = as.numeric(as.character(a)) - lag(as.numeric(as.character(a)), n=t_lag)) %>%
    mutate(acceleration = as.numeric(as.character(velocity)) - lag(as.numeric(as.character(velocity)), n=t_lag))
  return(scored)
}

# categorical label for tapping tasks
# 

# for summarising parsed, restructured data
summary_quick_tapping <- function(df, group_var) {
  TASK_NAME <- "QUICK_TAPPING"
  
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
              n.correct.taps = sum(correct_circle_tap == 1),
              n.correct.left_taps = sum(correct_circle_tap == 1 & target_circle == "LEFT"),
              n.correct.right_taps = sum(correct_circle_tap == 1 & target_circle == "RIGHT"),
              n.target.left_taps = sum(target_circle == "LEFT"),
              n.target.right_taps = sum(target_circle == "RIGHT"),
              n.total.taps = n()) %>%
    mutate(prop.correct.left_taps = n.correct.left_taps/n.target.left_taps,
           prop.correct.right_taps = n.correct.right_taps/n.target.right_taps,
           prop.correct.taps = n.correct.taps/n.total.taps)

  summary2.df <- df %>%
    group_by_(.dots = group_var) %>%
    select(c(1:19)) %>%
    summarise(median.first_tap_time = median(first_tap_time, na.rm=T),
              mean.first_tap_time = median(first_tap_time, na.rm=T),
              sd.first_tap_time = sd(first_tap_time, na.rm=T),
              min.first_tap_time = min(first_tap_time, na.rm=T),
              max.first_tap_time = max(first_tap_time, na.rm=T),
              
              median.total_taps = median(total_taps, na.rm=T),
              mean.total_taps = mean(total_taps, na.rm=T),
              sum.total_taps = sum(total_taps, na.rm=T),
              sd.total_taps = sd(total_taps, na.rm=T),
              min.total_taps = min(total_taps, na.rm=T),
              max.total_taps = max(total_taps, na.rm=T),
              
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
  
  final.df <- merge(summary.df,summary2.df)
  return(final.df)
}


# ===================================================================================
# SEQUENCE TAPPING
# ===================================================================================

# for scoring parsed restructured data
score_seq_tapping <- function(df, t_lag=3){
  scored <- df %>%
    mutate(correct_ordering = gsub("-", "", correct_order),
           response_order = gsub("-", "", gsub("OB", "", gsub("T","",gsub("HOME","",tap_sequence)))),
           perfect_order_recall = ifelse(correct_ordering == response_order, 1, 0)) %>%
    separate(home_center, into=c("home_center_x", "home_center_y"), " ", convert=T) %>%
    separate(target1center, into=c("target1_center_x", "target1_center_y"), " ", convert=T) %>%
    separate(target2center, into=c("target2_center_x", "target2_center_y"), " ", convert=T) %>%
    separate(target3center, into=c("target3_center_x", "target3_center_y"), " ", convert=T) %>%
    separate(target4center, into=c("target4_center_x", "target4_center_y"), " ", convert=T) %>%
    separate(target5center, into=c("target5_center_x", "target5_center_y"), " ", convert=T) %>%
    separate(target6center, into=c("target6_center_x", "target6_center_y"), " ", convert=T) %>%
    separate(target7center, into=c("target7_center_x", "target7_center_y"), " ", convert=T) %>%
    mutate(distance.from.home = distance(tap_x, home_center_x, tap_y, home_center_y),
           distance.from.target1 = distance(tap_x, target1_center_x, tap_y, target1_center_y),
           distance.from.target2 = distance(tap_x, target2_center_x, tap_y, target2_center_y),
           distance.from.target3 = distance(tap_x, target3_center_x, tap_y, target3_center_y),
           distance.from.target4 = distance(tap_x, target4_center_x, tap_y, target4_center_y),
           distance.from.target5 = distance(tap_x, target5_center_x, tap_y, target5_center_y),
           distance.from.target6 = distance(tap_x, target6_center_x, tap_y, target6_center_y),
           distance.from.target7 = distance(tap_x, target7_center_x, tap_y, target7_center_y)) %>%
    group_by(user_id,game_uuid,trial_num) %>%
    mutate(velocity = as.numeric(as.character(a)) - lag(as.numeric(as.character(a)), n=t_lag)) %>%
    mutate(acceleration = as.numeric(as.character(velocity)) - lag(as.numeric(as.character(velocity)), n=t_lag))
  return(scored)
}

# for summarising parsed, restructured data
summary_seq_tapping <- function(df, group_var, nontap_cols = c(1:24)) {
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
  
  final.df <- merge(summary.df,summary2.df)
  return(final.df)
}



# ===================================================================================
# COLOR DOTS
# ===================================================================================

# for scoring raw, parsed data
score_color_dots <- function(df, threshold=75){
  scored <- df %>%
    separate(Loc1, into=c("Loc1_x", "Loc1_y"), " ", convert=T) %>%
    separate(Loc2, into=c("Loc2_x", "Loc2_y"), " ", convert=T) %>%
    separate(Loc3, into=c("Loc3_x", "Loc3_y"), " ", convert=T) %>%
    separate(ProbedLocation, into=c("probe_x", "probe_y"), " ", convert=T) %>%
    separate(FinalLocation, into=c("final_x", "final_y"), " ", convert=T) %>%
    mutate(stage1.loc1_distance = distance(Loc1_x, probe_x, Loc1_y, probe_y),
           stage1.loc2_distance = distance(Loc2_x, probe_x, Loc2_y, probe_y),
           stage1.loc3_distance = distance(Loc3_x, probe_x, Loc3_y, probe_y)) %>%
    rowwise() %>%
    mutate(stage1.which.location.probed = which.min(c(stage1.loc1_distance, stage1.loc2_distance, stage1.loc3_distance))) %>%
    mutate(stage1.color.at.probed.location = ifelse(stage1.which.location.probed == 1,Col1,
                                      ifelse(stage1.which.location.probed == 2,Col2,
                                      ifelse(stage1.which.location.probed == 3, Col3, NA))),
           
           stage1.probe.location_x = ifelse(stage1.which.location.probed == 1, Loc1_x,
                                     ifelse(stage1.which.location.probed == 2, Loc2_x,
                                     ifelse(stage1.which.location.probed == 3, Loc3_x, NA))),
           
           stage1.probe.location_y = ifelse(stage1.which.location.probed == 1, Loc1_y,
                                     ifelse(stage1.which.location.probed == 2, Loc2_y,
                                     ifelse(stage1.which.location.probed == 3, Loc3_y, NA)))) %>%
    mutate(stage1.is.color.correct = ifelse(stage1.color.at.probed.location == ColorChoice, 1, 0)) %>%
    rowwise() %>%
    mutate(stage1.is.color.chosen.shown = ifelse(ColorChoice %in% c(Col1,Col2,Col3), 1, 0)) %>%
    mutate(stage1.classification = ifelse(stage1.is.color.chosen.shown == 1 & stage1.is.color.correct == 1, "CORRECT",
                                   ifelse(stage1.is.color.chosen.shown == 0 & stage1.is.color.correct == 0, "RANDOM",
                                   ifelse(stage1.is.color.chosen.shown == 1 & stage1.is.color.correct == 0, "SWAP")))) %>%
    mutate(stage2.col1_match = ifelse(ProbedColor == Col1, 1, 0),
           stage2.col2_match = ifelse(ProbedColor == Col2, 1, 0),
           stage2.col3_match = ifelse(ProbedColor == Col3, 1, 0)) %>%
    mutate(stage2.which.location.is.probed.color = which(1 == c(stage2.col1_match, stage2.col2_match, stage2.col3_match))) %>%
    mutate(stage2.location.at.probed.color.x = ifelse(stage2.which.location.is.probed.color == 1, Loc1_x,
                                               ifelse(stage2.which.location.is.probed.color == 2, Loc2_x,
                                               ifelse(stage2.which.location.is.probed.color == 3, Loc3_x, NA)))) %>%
    mutate(stage2.location.at.probed.color.y = ifelse(stage2.which.location.is.probed.color == 1, Loc1_y,
                                               ifelse(stage2.which.location.is.probed.color == 2, Loc2_y,
                                               ifelse(stage2.which.location.is.probed.color == 3, Loc3_y, NA)))) %>%
    mutate(stage2.loc1_distance = distance(Loc1_x, stage2.location.at.probed.color.x, 
                                           Loc1_y, stage2.location.at.probed.color.y),
           stage2.loc2_distance = distance(Loc2_x, stage2.location.at.probed.color.x, 
                                           Loc2_y, stage2.location.at.probed.color.y),
           stage2.loc3_distance = distance(Loc3_x, stage2.location.at.probed.color.x, 
                                           Loc3_y, stage2.location.at.probed.color.y),
           stage2.distance.from.color.probe = distance(final_x, stage2.location.at.probed.color.x, 
                                                            final_y, stage2.location.at.probed.color.y),
           stage2.distance.from.location.probe = distance(final_x, stage1.probe.location_x,
                                                       final_y, stage1.probe.location_y)) %>%
    mutate(which.location.unprobed = setdiff(c(1,2,3),c(stage1.which.location.probed, stage2.which.location.is.probed.color))) %>%     
    mutate(location.unprobed.x = ifelse(which.location.unprobed == 1, Loc1_x,
                                 ifelse(which.location.unprobed == 2, Loc2_x,
                                 ifelse(which.location.unprobed == 3, Loc3_x, NA))),
           location.unprobed.y = ifelse(which.location.unprobed == 1, Loc1_y,
                                 ifelse(which.location.unprobed == 2, Loc2_y,
                                 ifelse(which.location.unprobed == 3, Loc3_y, NA)))) %>%
    mutate(stage2.distance.from.location.unprobed = distance(final_x, location.unprobed.x,
                                                             final_y, location.unprobed.y)) %>%
    mutate(stage2.is.response.distance.near.color.probe = ifelse(stage2.distance.from.color.probe <= threshold, 1, 0),
           stage2.is.response.distance.near.location.probe = ifelse(stage2.distance.from.location.probe <= threshold, 1, 0),
           stage2.is.response.distance.near.location.unprobed = ifelse(stage2.distance.from.location.unprobed <= threshold, 1, 0)) %>%
    mutate(stage2.classification = ifelse(stage2.is.response.distance.near.color.probe == 1 & stage2.is.response.distance.near.location.probe == 0, "CORRECT",
                                   ifelse(stage2.is.response.distance.near.color.probe == 0 & stage2.is.response.distance.near.location.probe == 1, "SWAP",
                                   ifelse(stage2.is.response.distance.near.color.probe == 0 & stage2.is.response.distance.near.location.probe == 0, "RANDOM", NA))))

    
    return(scored)
}

# for processing COLOR DOTS data from Survey Dolphin
summary_color_dots <- function(df, group_var) {
  TASK_NAME <- "COLOR_DOTS"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.color = median(ColorRT,na.rm=T),
              sd.RT.color = sd(ColorRT,na.rm=T),
              
              median.RT.location = median(LocRT,na.rm=T),
              sd.RT.location = sd(LocRT,na.rm=T),
              
              stage2.median.precision.swap = median(stage2.distance.from.color.probe[stage2.classification == "SWAP"], na.rm=T),
              stage2.sd.precision.swap = sd(stage2.distance.from.color.probe[stage2.classification == "SWAP"], na.rm=T),
              
              stage2.median.precision.correct = median(stage2.distance.from.color.probe[stage2.classification == "CORRECT"], na.rm=T),
              stage2.sd.precision.correct = sd(stage2.distance.from.color.probe[stage2.classification == "CORRECT"], na.rm=T),
              
              stage2.median.precision.random = median(stage2.distance.from.color.probe[stage2.classification == "RANDOM"], na.rm=T),
              stage2.sd.precision.random = sd(stage2.distance.from.color.probe[stage2.classification == "RANDOM"], na.rm=T),
              
              stage2.median.precision.correct.or.swap = median(stage2.distance.from.color.probe[stage2.classification == "CORRECT" | stage2.classification == "SWAP"], na.rm=T),
              stage2.sd.precision.correct.or.swap = sd(stage2.distance.from.color.probe[stage2.classification == "CORRECT" | stage2.classification == "SWAP"], na.rm=T),
              
              stage1.swap.count = sum(stage1.classification == "SWAP"),
              stage2.swap.count = sum(stage2.classification == "SWAP"),
              
              stage1.random.count = sum(stage1.classification == "RANDOM"),
              stage2.random.count = sum(stage2.classification == "RANDOM"),
              
              stage1.correct.count = sum(stage1.classification == "CORRECT"),
              stage2.correct.count = sum(stage2.classification == "CORRECT"),
              
              n = n()) %>%
    mutate(stage1.swap.prop = stage1.swap.count / n,
           stage2.swap.prop = stage2.swap.count / n,
           
           stage1.random.prop = stage1.random.count / n,
           stage2.random.prop = stage2.random.count / n,
           
           stage1.correct.prop = stage1.correct.count / n,
           stage2.correct.prop = stage2.correct.count / n)
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}

# ===================================================================================
# VISUAL WORKING MEMORY
# ===================================================================================

# for scoring raw, parsed data
score_visual_wm <- function(df, threshold=15){
  scored <- df %>% 
    mutate(mc_response_correct = ifelse(user_response == target_response, 1, 0))
  return(scored)
}


summary_visual_wm <- function(df, group_var) {
  TASK_NAME <- "VISUAL_WM"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time,na.rm=T),
              mean.RT.all_trials = mean(response_time,na.rm=T),
              sd.RT.all_trials = sd(response_time,na.rm=T),
              
              median.RT.multiple_choice_2opt = median(response_time[response_type == "MULTI_CHOICE_2"],na.rm=T),
              mean.RT.multiple_choice_2opt = mean(response_time[response_type == "MULTI_CHOICE_2"],na.rm=T),
              sd.RT.multiple_choice_2opt = sd(response_time[response_type == "MULTI_CHOICE_2"],na.rm=T),
              
              median.RT.multiple_choice_3opt = median(response_time[response_type == "MULTI_CHOICE_3"],na.rm=T),
              mean.RT.multiple_choice_3opt = mean(response_time[response_type == "MULTI_CHOICE_3"],na.rm=T),
              sd.RT.multiple_choice_3opt = sd(response_time[response_type == "MULTI_CHOICE_3"],na.rm=T),
              
              median.RT.free_rotate = median(response_time[response_type == "FREE_ROTATE"],na.rm=T),
              mean.RT.free_rotate = mean(response_time[response_type == "FREE_ROTATE"],na.rm=T),
              sd.RT.free_rotate = sd(response_time[response_type == "FREE_ROTATE"],na.rm=T),
              
              n.multiple_choice_correct = sum(mc_response_correct, na.rm=T),
              
              n.multiple_choice_trials = sum(response_type == "MULTI_CHOICE_2" | response_type == "MULTI_CHOICE_3"),
              n.multiple_choice_2opt_trials = sum(response_type == "MULTI_CHOICE_2"),
              n.multiple_choice_3opt_trials = sum(response_type == "MULTI_CHOICE_3"),
              n.free_rotate_trials = sum(response_type == "FREE_ROTATE"),
              n.trials = n()) %>%
    mutate(prop.multiple_choice.correct = n.multiple_choice_correct / n.multiple_choice_trials)
}

