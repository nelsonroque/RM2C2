#' RM2C2: Scoring, Summarizing

#' @name filter_cog_duplicate_records
#' @param df class: numeric; original X
#' @param group_vars class: vector; factors to group data by
#' @param time_var class: vector; variable for time
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' filter_cog_duplicate_records(df)
#' @export

filter_cog_duplicate_records <- function(df) {
    return(df %>% distinct())
}