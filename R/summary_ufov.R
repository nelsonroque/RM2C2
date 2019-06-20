#' RM2C2: Scoring, Summarizing

#' @name summary_ufov
#' @export
summary_ufov <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "UFOV"
  summary.df <- df %>%
    group_by(.dots = c(group_var,'central_image_category', 'param_target_disp_time')) %>%
    summarise(mean.peripheral.distance = mean(peripheral_accuracy_dist[did_peripheral_response_timeout == 0], na.rm=T),
              median.peripheral.distance = median(peripheral_accuracy_dist[did_peripheral_response_timeout == 0], na.rm=T),
              
              mean.central.response_time = mean(central_response_time, na.rm=T),
              median.central.response_time = median(central_response_time, na.rm=T),
              sd.central.response_time = sd(central_response_time, na.rm=T),
              
              mean.peripheral.response_time = mean(peripheral_response_time, na.rm=T),
              median.peripheral.response_time = median(peripheral_response_time, na.rm=T),
              sd.peripheral.response_time = sd(peripheral_response_time, na.rm=T),
              
              mean.fixation_display_time_deviance = mean(fixation_display_time_deviance, na.rm=T),
              mean.target_display_time_deviance = mean(target_display_time_deviance, na.rm=T),
              mean.mask_display_time_deviance = mean(mask_display_time_deviance, na.rm=T),
              
              mean.peripheral.down_responses = mean(peripheral_down_count, na.rm=T),
              mean.peripheral.up_responses = mean(peripheral_up_count, na.rm=T),
              sum.peripheral.down_responses = sum(peripheral_down_count, na.rm=T),
              sum.peripheral.up_responses = sum(peripheral_up_count, na.rm=T),
              
              n.central_correct.trials = sum((central_accuracy == 1)),
              n.central_incorrect.trials = sum((central_accuracy == 0)),
              n.central_timeout.trials = sum((did_central_response_timeout == 1)),
              n.peripheral_timeout.trials = sum((did_peripheral_response_timeout == 1)),
              n.trials = n()) %>%
    mutate(ratio.count.peripheral.down_up.responses = sum.peripheral.down_responses/sum.peripheral.up_responses) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
    
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}
