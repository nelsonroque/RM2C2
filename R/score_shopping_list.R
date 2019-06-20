#' RM2C2: Scoring, Summarizing

#' @name score_shopping list
#' @export
score_shopping_list <- function(df){
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    mutate(correct = ifelse(target_price == choice, 1, 0),
           choice_RT = choiceRT) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}