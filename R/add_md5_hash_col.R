#' RM2C2: Scoring, Summarizing

#' @name add_md5_hash_col
#' @param data class: data.frame
#' @param algo class: string
#' @import tidyverse
#' @import digest
#' @examples
#' add_md5_hash_col(data, algo="md5")
#' @export
add_md5_hash_col <- function(data, algo="md5") {
  data_md5 <- digest::digest(data, algo=algo)
  return(data <- data %>% mutate(RM2C2_processing_hash = data_md5,
                                 RM2C2_processing_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
}