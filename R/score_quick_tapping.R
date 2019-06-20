#' RM2C2: Scoring, Summarizing

#' @name score_quick_tapping
#' @export
score_quick_tapping <- function(df, t_lag=3){
  PACKAGE.VERSION <- packageVersion("RM2C2")
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
    mutate(acceleration = as.numeric(as.character(velocity)) - lag(as.numeric(as.character(velocity)), n=t_lag)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}
