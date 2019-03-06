score_go_nogo <- function(df, nogo_letter = "X") {
  scored <- df %>%
    mutate(no.go.letter = nogo_letter,
           response_time = ifelse(responseTime == -1,NA,responseTime)) %>%
    mutate(is.nogo.frame = ifelse(letter == no.go.letter, 1, 0),
           FA = ifelse(is.nogo.frame == 1 & response == 1,1,0),
           MISS = ifelse(is.nogo.frame == 0 & response == 0,1,0),
           HIT = ifelse(is.nogo.frame == 0 & response == 1,1,0),
           CR = ifelse(is.nogo.frame == 1 & response == 0,1,0)) 
  return(scored)
}