#' RM2C2: Scoring, Summarizing

#' @name score_symbol_search
#' @export
score_symbol_search <- function(df) {
  scored <- df %>% mutate(accuracy = ifelse(user_response == correct_response,1,0))
  return(scored)
}