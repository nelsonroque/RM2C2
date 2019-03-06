#' RM2C2: Scoring, Summarizing

#' @name get_file_ext
#' @param filepath class: string
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' get_file_ext("C:/filepath.csv")

#' @export

get_file_ext <- function(filepath) {
  file.info = strsplit(filepath,"\\.")
  file.ext = file.info[[1]][2]
  return(file.ext)
}