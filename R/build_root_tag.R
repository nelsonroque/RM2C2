#' RM2C2: Scoring, Summarizing

#' @name build_root_tag
#' @param df class: dataframe
#' @param df class: string
#' @param root_tag  class: string
#' @keywords m2c2, cognition
#' @examples
#' build_root_tag(df, root_tag)
#' @export
build_root_tag <- function(df, root_tag) {
  
  # read root tag
  root_s = xmlTreeParse(root_tag, useInternalNodes = T)
  
  # build root `Dolphin` tag
  # ______________________________________________________
  root = xmlRoot(root_s)
  
  # filter the dolphin dataframe for what should be two attributes
  nd <- df %>% select_if(function(x) any(!is.na(x)))
  
  # add attributes to the root
  xmlAttrs(root) = nd
  
  return(root)
}