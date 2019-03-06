#' RM2C2: Scoring, Summarizing

#' @name filter_studyday
#' @param df class: numeric; original X
#' @param study_col class: string
#' @param start_day class: numeric; moved to X
#' @param end_day class: numeric; moved to X
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' filter_studyday(df,study_col='date', start_day="2019-02-01", end_day="2019-03-01")
#' @export

filter_studyday <- function(df,study_col='studyday',start_day=1,end_day=14) {
  filtered <- df %>%
    filter(study_col >= start_day) %>%
    filter(study_col <= end_day)
  return(filtered)
}