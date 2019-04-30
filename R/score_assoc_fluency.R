#' RM2C2: Scoring, Summarizing

#' @name score_assoc_fluency
#' @export
score_assoc_fluency <- function(df) {
  print("NOTE: Script currently only supports up to 5 entries.")
  result <- df %>% 
    mutate(is_entry1 = ifelse(is.na(entry1), 0, 1),
           is_entry2 = ifelse(is.na(entry2), 0, 1),
           is_entry3 = ifelse(is.na(entry3), 0, 1),
           is_entry4 = ifelse(is.na(entry4), 0, 1),
           is_entry5 = ifelse(is.na(entry5), 0, 1)) %>%
    rowwise() %>%
    mutate(total_entries = sum(c(is_entry1, is_entry2, is_entry3, is_entry4, is_entry5)))
  return(result)
}