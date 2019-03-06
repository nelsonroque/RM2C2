#' RM2C2: Scoring, Summarizing

#' @name add_to
#' @param x class: numeric; original number
#' @param add class: numeric; number to add
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' add_to(x1,x2,y1,y2)
#' @export
#' 
add_to <- function(x,add=1) {
  x1 <- x + add
  return(x1)
}