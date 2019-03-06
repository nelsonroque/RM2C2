#' RM2C2: Scoring, Summarizing

#' @name studyday_per_participant
#' @param df class: data frame
#' @param id_var class: string
#' @param session_var class: string
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' studyday_per_participant(df,"user_id","studyday")

#' @export
studyday_per_participant <- function(df,id_var,studyday_var) {
  n.sessions <- df %>%
    group_by_(.dots = id_var) %>%
    summarise_(n.unique.studydays = interp(~length(unique(var)), var = as.name(studyday_var)),
               min.studyday = interp(~min(var, na.rm = TRUE), var = as.name(studyday_var)),
               max.studyday = interp(~max(var, na.rm = TRUE), var = as.name(studyday_var)))
  return(n.sessions)
}