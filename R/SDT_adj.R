#' RM2C2: Scoring, Summarizing

#' @name SDT_adj
#' @param df class: dataframe
#' @param adj class: numeric; SDT adjustment
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' SDT_adj(df, adj=0.01)
#' @export

SDT_adj <- function(df, adj=0.01) {
  up_adj = 1 - adj
  down_adj = 0 + adj
  
  out.df <- df %>%
    mutate(CorRec.rate = HIT.rate - FA.rate) %>%
    mutate(HIT.rate.adj = ifelse(HIT.rate == 1, up_adj, HIT.rate),
           FA.rate.adj = ifelse(FA.rate == 1, up_adj, FA.rate),
           MISS.rate.adj = ifelse(MISS.rate == 1, up_adj, MISS.rate),
           CR.rate.adj = ifelse(CR.rate == 1, up_adj, CR.rate)) %>%
    mutate(HIT.rate.adj = ifelse(HIT.rate == 0, down_adj, HIT.rate.adj),
           FA.rate.adj = ifelse(FA.rate == 0, down_adj, FA.rate.adj),
           MISS.rate.adj = ifelse(MISS.rate == 0, down_adj, MISS.rate.adj),
           CR.rate.adj = ifelse(CR.rate == 0, down_adj, CR.rate.adj)) %>%
    mutate(z.HIT.rate = qnorm(HIT.rate.adj),
           z.FA.rate = qnorm(FA.rate.adj),
           z.MISS.rate = qnorm(MISS.rate.adj),
           z.CR.rate = qnorm(CR.rate.adj)) %>%
    mutate(dprime = z.HIT.rate - z.FA.rate,
           beta = exp(-z.HIT.rate * z.HIT.rate / 2 + z.FA.rate * z.FA.rate / 2),
           c = -(z.HIT.rate + z.FA.rate) / 2)
  return(out.df)
}
