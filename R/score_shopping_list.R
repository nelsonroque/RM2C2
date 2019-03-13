#' RM2C2: Scoring, Summarizing

#' @name score_shopping list
#' @export
score_shopping_list <- function(df){
  scored <- shopping_list %>%
    mutate(correct = ifelse(target_price == choice, 1, 0),
           choice_RT = choiceRT)
  return(scored)
}