#' RM2C2: Scoring, Summarizing

#' @name mult_by
#' @param x class: numeric; original number
#' @param add class: numeric; number to add
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' mult_by(x, mult=5)
#' @export
#' 
mult_by <- function(x,mult=1) {
  x1 <- x * mult
  return(x1)
}