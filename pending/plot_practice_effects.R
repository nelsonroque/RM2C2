library(tidyverse)
library(RM2C2)


plot_practice_effect <- function(df = NA, filename = NA, y_var = NA, time_var = NA) {

  pdf(file=filename)  
  
  unique.ids <- eval(expr(unique(df$idc)))
  
  for(i in unique.ids) {
    
    # subset data
    temp.df <- df %>% 
      filter(idc == i) %>%
      group_by(idc, studyday) %>%
      summarise(m = mean(cs_pr, na.rm=T))
    
    # draw plot
    p <- ggplot(temp.df, aes(studyday, m)) + 
      geom_point() +
      geom_smooth(method="lm") +
      ylim(-1,1) +
      xlim(-5,30) +
      ggtitle(paste0("Participant ID: ",i))
    
    print(p)
    
    # for debugging
    print(i)
    print(nrow(temp.df))
    print("-----")
    
    # remove old data
    rm(temp.df)
  }
  
  dev.off() 
  
}