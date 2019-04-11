library(tidyverse)
library(RM2C2)


plot_practice_effect <- function(df = NA, filename = NA, y_var = NA, id_var = NA, time_var = NA) {

  # get unique ids
  qid <- quo(id_var)
  unique.ids <- df %>% select(!! qid) %>% distinct(!! qid) %>% distinct()
  
  # open PDF
  pdf(file=filename)  
  
  for(i in unique.ids) {
    # get summary of person series
    summary_method <- paste0("mean(",id_var, ",na.rm=T)")
    
    # subset data
    temp.df <- df %>% 
      filter(!! qid == i) %>%
      group_by_at(c(id_var, time_var)) %>%
      summarise_(.dots = set_names(summary_method, "value"))
    
    # draw plot
    p <- ggplot(temp.df, aes(time_var, value)) + 
      geom_point() +
      geom_smooth(method="lm") +
      #ylim(-1,1) +
      #xlim(-5,30) +
      ggtitle(paste0("Participant ID: ",i))
    
    # display plot in PDF
    print(p)
    
    # for debugging
    print(summary_method)
    print(i)
    print(nrow(temp.df))
    print("-----")
    
    # remove old data
    rm(temp.df)
  }
  
  #dev.off() 
  
}

plot_practice_effect(df, filename="test.pdf", y_var="spat_dmn", id_var="id", time_var="studyday")
