#' RM2C2: Scoring, Summarizing

#' @name span_order
#' @param df class: dataframe
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' span_order(df)

#' @export
span_order <- function(df) {
  # there has to be a better way to get max items in this data brick, without iteratively checking all column names
  
  if("mem5" %in% names(df)){
    out.df <- df %>%
      mutate(r1_order_correct = ifelse(mem1 == Choice1, 1, 0),
             r2_order_correct = ifelse(mem2 == Choice2, 1, 0),
             r3_order_correct = ifelse(mem3 == Choice3, 1, 0),
             r4_order_correct = ifelse(mem4 == Choice4, 1, 0),
             r5_order_correct = ifelse(mem5 == Choice5, 1, 0)) %>%
      rowwise() %>%
      mutate(sum_order_correct = sum(c(r1_order_correct, r2_order_correct, r3_order_correct, r4_order_correct, r5_order_correct), na.rm=T))
    
  } else {
    if("mem4" %in% names(df)) {
      out.df <- df %>%
        mutate(r1_order_correct = ifelse(mem1 == Choice1, 1, 0),
               r2_order_correct = ifelse(mem2 == Choice2, 1, 0),
               r3_order_correct = ifelse(mem3 == Choice3, 1, 0),
               r4_order_correct = ifelse(mem4 == Choice4, 1, 0)) %>%
        rowwise() %>%
        mutate(sum_order_correct = sum(c(r1_order_correct, r2_order_correct, r3_order_correct, r4_order_correct), na.rm=T))
      
    }
  }
  
  out.df <- out.df %>%
    mutate(perfect.order.recall = ifelse(sum_order_correct == set_size, 1, 0))
  
  return(out.df)
}
