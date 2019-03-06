restructure_tapping_data <- function(df, nontap_cols = c(1:17), tap_col = c(18)) {
  restructured <- data.frame()
  for(row in 1:nrow(df)){
    row.values.nontap <- df[row,nontap_cols]
    row.values.tap <- df[row,tap_col]
    vals <- strsplit(unlist(strsplit(row.values.tap$tap_values,"\\+")),"_")
    vals.df <- data.frame(t(sapply(vals,c)))
    names(vals.df) <- c("tap_x","tap_y","a", "b", "c", "d")
    vals.df$tap_number <- seq(1,nrow(vals.df),1)
    export.row <- cbind(row.values.nontap,vals.df)
    restructured <- rbind(restructured,export.row)
  }
  
  restructured <- restructured %>%
    mutate(tap_x = as.numeric(as.character(tap_x)),
           tap_y = as.numeric(as.character(tap_y)))
  
  return(restructured)
}