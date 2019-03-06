#' @export
score_shopping_list <- function(df){
  scored <- shopping_list %>%
    mutate(correct = ifelse(target_price == choice, 1, 0),
           judgement_RT = ifelse(judgement_RT == ".", NA, as.numeric(judgement_RT)),
           choice_RT = ifelse(choiceRT == ".", NA, as.numeric(choiceRT)),
           session_id = session_id[1])
  return(scored)
}