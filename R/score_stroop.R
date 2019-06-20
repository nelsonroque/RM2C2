#' RM2C2: Scoring, Summarizing

#' @name score_stroop
#' @export
score_stroop <- function(df) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  return(scored)
}
