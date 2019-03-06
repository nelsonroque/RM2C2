#' @export
score_symbol_search <- function(df) {
  scored <- df %>% mutate(accuracy = ifelse(user_response == correct_response,1,0))
  return(scored)
}