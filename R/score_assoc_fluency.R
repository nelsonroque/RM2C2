#' RM2C2: Scoring, Summarizing

#' @name score_assoc_fluency
#' @export
score_assoc_fluency <- function(df) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  max_size <- get_max_col(df, colstring="entry")
  set_seq <- seq(1,max_size,1)
  
  for(i in 1:max_size){
    varname <- paste0("is_entry", i)
    new_method <- paste0("ifelse(is.na(entry",i,"), 0, 1)")
    df <- df %>% mutate_(.dots = setNames(new_method, varname))
  }
  
  # sum correct distractor responses
  varname2 <- "total_entries"
  new_method2 <- paste0("sum(",paste0("is_entry",set_seq, collapse=","), ",na.rm=T)")
  
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method2, varname2))
  
  scored <-  df %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)

  return(scored)
}