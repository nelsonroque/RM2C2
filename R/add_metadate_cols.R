#' RM2C2: Scoring, Summarizing

#' @name add_metadate_cols
#' @param df class: dataframe
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' add_metadate_cols(df)
#' @export
add_metadate_cols <- function(df) {
  df.w.dates <- df %>%
    mutate(current_datetime = as.POSIXct(time_stamp, origin = "1960-01-01")) %>%
    mutate(WEEK = lubridate::week(current_datetime),
           MONTH = lubridate::month(current_datetime),
           DAY = lubridate::day(current_datetime),
           YEAR = lubridate::year(current_datetime),
           TIME_HMS = lubridate::hms(current_datetime),
           TIME_HOUR = lubridate::hour(current_datetime),
           WEEKDAY.value = lubridate::wday(current_datetime),
           WEEKDAY.label = lubridate::wday(current_datetime,label=T)) %>%
    mutate(WEEKEND = ifelse(WEEKDAY.label == "Sat" | WEEKDAY.label == "Sun",1,0)) %>%
    mutate(TIME_WINDOW_2hr = cut(TIME_HOUR,
                                 breaks=c(0,8,10,12,14,16,18,20,22,24), 
                                 include.lowest=TRUE, 
                                 right=FALSE))
  return(df.w.dates)
}