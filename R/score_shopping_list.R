#' RM2C2: Scoring, Summarizing

#' @name score_shopping list
#' @export
score_shopping_list <- function(df, app_version = "1_1_release_18_7_27"){
  PACKAGE.VERSION <- packageVersion("RM2C2")
  if(app_version != "1_1_release_18_7_27") {
    scored <- df %>%
      mutate(correct = ifelse(target_price == choice, 1, 0),
             choice_RT = choiceRT) %>%
      mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  } else {
    scored <- df %>%
      mutate(correct = ifelse(target_price == choice, 1, 0)) %>%
      mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  }

  return(scored)
}