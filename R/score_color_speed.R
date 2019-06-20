#' RM2C2: Scoring, Summarizing

#' @name score_color_speed
#' @export
score_color_speed <- function(df, threshold=75) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>% 
    mutate(color_accuracy = ifelse(color == ColorChoice, 1, 0)) %>%
    separate(Loc1, into=c("cue_x", "cue_y"), " ", convert=T) %>%
    separate(TouchLocation, into=c("touch_x", "touch_y"), " ", convert=T) %>%
    mutate(location.precision = distance(as.numeric(touch_x), as.numeric(cue_x), as.numeric(touch_y), as.numeric(cue_y))) %>%
    mutate(is_location_response_outbounds = ifelse(location.precision > threshold, 1, 0)) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}