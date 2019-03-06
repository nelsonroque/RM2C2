#' RM2C2: Scoring, Summarizing

#' @name distance
#' @param x1 class: numeric; original X
#' @param x2 class: numeric; moved to X
#' @param y1 class: numeric; original Y
#' @param y2 class: numeric; moved to Y
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' distance(x1,x2,y1,y2)
#' @export

distance <- function(x1,x2,y1,y2) {
  d = sqrt(((x2 - x1)^2 + (y2 - y1)^2))
  return(d)
}