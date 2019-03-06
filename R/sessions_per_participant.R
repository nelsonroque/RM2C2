#' RM2C2: Scoring, Summarizing

#' @name sessions_per_participant
#' @param df class: data frame
#' @param id_var class: string
#' @param session_var class: string
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' sessions_per_participant(df,"user_id","session_id")

#' @export
sessions_per_participant <- function(df,id_var,session_var) {
  n.sessions <- df %>%
    group_by_(.dots = id_var) %>%
    summarise_(n.unique.sessions = interp(~length(unique(var)), var = as.name(session_var)),
               min.session = interp(~min(var, na.rm = TRUE), var = as.name(session_var)),
               max.session = interp(~max(var, na.rm = TRUE), var = as.name(session_var)))
  return(n.sessions)
}