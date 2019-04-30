#' RM2C2: Scoring, Summarizing

#' @name score_assoc_fluency
#' @export
score_assoc_fluency <- function(df) {
  max_size <- get_max_col(df, colstring="entry")
  set_seq <- seq(1,max_size,1)
  
  for(i in 1:max_size){
    varname <- paste0("is_entry", i)
    new_method <- paste0("ifelse(is.na(entry",i,"), 0, 1)")
    print(new_method)
    df <- df %>% mutate_(.dots = setNames(new_method, varname))
  }
  return(df)
}