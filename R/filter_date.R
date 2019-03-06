#' RM2C2: Scoring, Summarizing

#' @name filter_date
#' @param df class: numeric; original X
#' @param date_col class: numeric; moved to X
#' @param start_day class: numeric; moved to X
#' @param end_day class: numeric; moved to X
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' filter_date(df,date_col='date', start_day="2019-02-01", end_day="2019-03-01")
#' @export

filter_date <- function(df,date_col='date',start_day="",end_day="") {
  if(start_day == "" & end_day == "") {
    print("ERROR: no start or end date set")
  } else{
    filtered <- df %>%
      filter(date_col >= start_day) %>%
      filter(date_col <= end_day)
  }
  return(filtered)
}
