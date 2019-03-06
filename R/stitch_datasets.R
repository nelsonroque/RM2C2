#' RM2C2: Scoring, Summarizing

#' @name read_ambcog
#' @param df_list class: list of dataframes
#' @param common_id class: vector of strings
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' stitch_datasets(df_list, common_id, outcomes = NULL)

#' @export
stitch_datasets <- function(df_list, common_id) {
  return(df_list %>% reduce(left_join, by = common_id))
}