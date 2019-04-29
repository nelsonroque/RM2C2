#' RM2C2: Scoring, Summarizing

#' @name add_common_ids
#' @param df class: data frame
#' @param cols class: named list
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' add_common_ids(df, cols=list(burst="1"))

#' @export
add_common_ids <- function(df, cols=NA, verbose=F) {
  for(i in 1:length(cols)){
    
    # for each column in list, extract data
    colname <- names(cols[i])
    colvalue <- as.character(cols[i]) # as.character issues ... 
    
    # echo values for debugging
    if(verbose){
      print(colname)
      print(colvalue)
    }
    
    # add each column name / value
    df <- df %>%
      mutate(!!colname := colvalue)
  }
  return(df)
}