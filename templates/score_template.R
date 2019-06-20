#' RM2C2: Scoring, Summarizing

#' @name score_
#' @export
score_ <- function(df) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    mutate() %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}
