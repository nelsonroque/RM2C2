#' RM2C2: Scoring, Summarizing

#' @name score_seq_tapping
#' @export
score_seq_tapping <- function(df, t_lag=3){
  PACKAGE.VERSION <- packageVersion("RM2C2")
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
    mutate(acceleration = as.numeric(as.character(velocity)) - lag(as.numeric(as.character(velocity)), n=t_lag)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}