#' RM2C2: Scoring, Summarizing

#' @name filter_exact_duplicates
#' @param df class: numeric; original X
#' @param group_vars class: vector; factors to group data by
#' @param time_var class: vector; variable for time
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' filter_exact_duplicates(df, group_vars=c("user_id", "Session"))
#' @export

filter_exact_duplicates <- function(df, group_vars=c("user_id", "Session")) {
  arrange.var <- c(group_vars)
  filter.df <- df %>% distinct()
  return(filter.df)
}