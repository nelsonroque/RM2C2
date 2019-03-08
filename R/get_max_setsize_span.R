#' RM2C2: Scoring, Summarizing

#' @name get_max_setsize_span
#' @export
get_max_setsize_span <- function(df) {
  regexp <- "[[:digit:]]+"
  max.mem.item <- df %>% 
    select(contains("mem")) %>%
    gather(variable, value) %>%
    mutate(max_items = str_extract(variable, regexp)) %>%
    summarise(max = max(max_items))
  return(as.numeric(max.mem.item))
}