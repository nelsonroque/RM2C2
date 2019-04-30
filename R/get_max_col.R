#' RM2C2: Scoring, Summarizing

#' @name get_max_col
#' @export
get_max_col <- function(df, colstring = NA) {
  regexp <- "[[:digit:]]+"
  max.item <- df %>% 
    select(contains(colstring)) %>%
    gather(variable, value) %>%
    mutate(max_items = str_extract(variable, regexp)) %>%
    summarise(max = max(max_items))
  return(as.numeric(max.item))
}