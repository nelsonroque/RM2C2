#' RM2C2: Scoring, Summarizing

#' @name compliance_report
#' @param df class: numeric; original X
#' @param id_var class: string
#' @param session_var class: numeric; moved to X
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' compliance_report(df)
#' @export

compliance_report <- function(df, id_var = "ID", session_var = "session", id_cols = c(1:20)) {
  report.df <- df %>% 
    group_by_(id_var) %>%
    summarise(n.records = n(),
              n.unique.sessions = length(unique(session)))
  return(report.df)
}