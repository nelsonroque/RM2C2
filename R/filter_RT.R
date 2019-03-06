#' RM2C2: Scoring, Summarizing

#' @name filter_RT
#' @param df class: numeric; original X
#' @param RT_col class: string
#' @param lower_RT class: numeric; moved to X
#' @param upper_RT class: numeric; moved to X
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' filter_RT(df, RT_col = 'response_time', lower_RT = 200, upper_RT = 10000)
#' @export

filter_RT <- function(df, RT_col = 'response_time', lower_RT = 200, upper_RT = 10000) {
  filtered <- df %>%
    mutate(response_time = replace(response_time, which(response_time <= lower_RT), NA)) %>%
    mutate(response_time = replace(response_time, which(response_time >= upper_RT), NA))
  return(filtered)
}