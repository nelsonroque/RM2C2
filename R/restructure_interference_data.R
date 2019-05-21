#' RM2C2: Scoring, Summarizing

#' @name restructure_interference_data
#' @export
restructure_interference_data <- function(df, nontap_cols = c(1:13), tap_col = c(14)) {
  restructured <- data.frame()
  for(row in 1:nrow(df)){
    row.values.nontap <- df[row,nontap_cols]
    row.values.tap <- df[row,tap_col]

    raw.vals <- data.frame(il = unlist(strsplit(row.values.tap$interference_locations, " ")))
    vals.df <- raw.vals %>% separate(il, c("timestamp", "interf_page", "tap_row", "tap_column", "tap_correct", "pixel_x", "pixel_y"))
    
    vals.df$tap_number <- seq(1,nrow(vals.df),1)
    export.row <- cbind(row.values.nontap,vals.df)
    restructured <- rbind(restructured,export.row)
  }
  
  restructured <- restructured %>%
    mutate(interf_page = as.numeric(as.character(interf_page)),
           tap_row = as.numeric(as.character(tap_row)),
           tap_column = as.numeric(as.character(tap_column)),
           pixel_x = as.numeric(as.character(pixel_x)),
           pixel_y = as.numeric(as.character(pixel_y)),
           tap_correct = as.numeric(as.character(tap_correct))) %>%
    mutate(is_data_restructured = TRUE)
  
  return(restructured)
}