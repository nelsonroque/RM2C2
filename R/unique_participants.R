#' RM2C2: Scoring, Summarizing

#' @name unique_participants
#' @param vec class: vector of data
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' unique_participants(df$user_id)

#' @export

unique_participants <- function(vec) {
  print(length(unique(vec)))
}