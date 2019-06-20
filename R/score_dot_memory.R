#' RM2C2: Scoring, Summarizing

#' @name score_dot_memory
#' @export
score_dot_memory <- function(df, square_size=5) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  APK.VERSION <- "version:1.0|candidate:D|after:03_18_2018|commit_id:f8d4de974b5bebfa32d72382ac81911b950b4b75"
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
    mutate(r1_d1_distance = distance(user_dot1_rx_1_coord, dot1_rx_1_coord,
                                  user_dot1_ry_1_coord, dot1_ry_1_coord),
           r1_d2_distance = distance(user_dot1_rx_1_coord, dot2_rx_1_coord,
                                     user_dot1_ry_1_coord, dot2_ry_1_coord),
           r1_d3_distance = distance(user_dot1_rx_1_coord, dot3_rx_1_coord,
                                     user_dot1_ry_1_coord, dot3_ry_1_coord),
           
           r2_d1_distance = distance(user_dot2_rx_1_coord, dot1_rx_1_coord,
                                     user_dot2_ry_1_coord, dot1_ry_1_coord),
           r2_d2_distance = distance(user_dot2_rx_1_coord, dot2_rx_1_coord,
                                     user_dot2_ry_1_coord, dot2_ry_1_coord),
           r2_d3_distance = distance(user_dot2_rx_1_coord, dot3_rx_1_coord,
                                     user_dot2_ry_1_coord, dot3_ry_1_coord),
           
           r3_d1_distance = distance(user_dot3_rx_1_coord, dot1_rx_1_coord,
                                     user_dot3_ry_1_coord, dot1_ry_1_coord),
           r3_d2_distance = distance(user_dot3_rx_1_coord, dot2_rx_1_coord,
                                     user_dot3_ry_1_coord, dot2_ry_1_coord),
           r3_d3_distance = distance(user_dot3_rx_1_coord, dot3_rx_1_coord,
                                     user_dot3_ry_1_coord, dot3_ry_1_coord)) %>%
    rowwise() %>%
    mutate(r1_which = which.min(c(r1_d1_distance, r1_d2_distance, r1_d3_distance)),
           r2_which = which.min(c(r2_d1_distance, r2_d2_distance, r2_d3_distance)),
           r3_which = which.min(c(r3_d1_distance, r3_d2_distance, r3_d3_distance))) %>%
    mutate(r1_distance = min(c(r1_d1_distance, r1_d2_distance, r1_d3_distance)),
           r2_distance = min(c(r2_d1_distance, r2_d2_distance, r2_d3_distance)),
           r3_distance = min(c(r3_d1_distance, r3_d2_distance, r3_d3_distance))) %>%
    mutate(r1_perfect = ifelse(r1_distance == 0, 1, 0),
           r2_perfect = ifelse(r2_distance == 0, 1, 0),
           r3_perfect = ifelse(r3_distance == 0, 1, 0),
           perfect_response = ifelse(r1_distance == 0 & r2_distance == 0 & r3_distance == 0,1,0)) %>%
    rowwise() %>%
    mutate(sum_perfect_dots = sum(c(r1_perfect, r2_perfect, r3_perfect)),
           median_error_distance = median(c(r1_distance, r2_distance, r3_distance)),
           sum_error_distance = sum(c(r1_distance, r2_distance, r3_distance))) %>%
    mutate(scoring_script_apk_match = APK.VERSION) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
    
  return(scored)
}
