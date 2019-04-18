library(tidyverse)
library(readr)

# sample experience recreation dataset
df <- read_delim("objdata_6.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)

# get random trial
single.trial <- df %>% filter(trial == sample(0:max(df$trial),size=1))

# plot trial data
ggplot(single.trial, aes(x,y,color=obj.color,shape=obj_identity)) + 
  geom_point(size=9) +
  scale_colour_identity()+
  theme_bw() + 
  ggtitle(paste0("Trial #: ", single.trial$trial))
