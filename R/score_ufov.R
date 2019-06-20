#' RM2C2: Scoring, Summarizing

#' @name score_ufov
#' @export
score_ufov <- function(df) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    mutate(fixation_display_time_deviance = param_fixation_disp_time - actual_fixation_disp_time,
           target_display_time_deviance = param_target_disp_time - actual_target_disp_time,
           mask_display_time_deviance = param_mask_disp_time - actual_mask_disp_time) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}
