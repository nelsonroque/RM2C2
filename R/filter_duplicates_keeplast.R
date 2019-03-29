#' RM2C2: Scoring, Summarizing

#' @name filter_survey_duplicate_records
#' @param df class: numeric; original X
#' @param group_vars class: vector; factors to group data by
#' @param time_var class: vector; variable for time
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' filter_duplicates_keeplast(df, group_vars=c("user_id", "Session"), time_var="End Time")
#' @export

filter_survey_duplicate_records <- function(df, group_vars=c("user_id", "Session"), time_var="End Time") {
  arrange.var <- c(group_vars, time_var)
  order.df <- df %>%
    arrange_(., c(arrange.var))
  
  filter.df <- order.df[ !duplicated(order.df[, group_vars], fromLast=T),]
  return(filter.df)
}