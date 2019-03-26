#' RM2C2: Scoring, Summarizing

#' @name compliance_report
#' @param df class: numeric; original X
#' @param ... class string, vector
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' compliance_report(df)
#' @export

compliance_report <- function(df, ...) {
  group_var <- enquos(...)
  
  report.df <- df %>%
    group_by(!!! group_var) %>%
    summarise(n.records = n())
  return(report.df)
}