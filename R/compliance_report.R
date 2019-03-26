compliance_report <- function(df, id_var = "ID", session_var = "session", id_cols = c(1:20)) {
  report.df <- df %>% 
    group_by_(id_var) %>%
    summarise(n.records = n(),
              n.unique.sessions = length(unique(session)))
  return(report.df)
}

compliance_report(all.surveys)
