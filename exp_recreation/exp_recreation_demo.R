library(tidyverse)
library(readr)

# sample experience recreation dataset
df <- read_delim("objdata_6.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(isChangeTrial = ifelse(isChangeTrial == 1, "CHANGE", "ORIGINAL")) %>%
  mutate(change_trial = factor(isChangeTrial, levels=c("ORIGINAL","CHANGE")))

# get random trial
single.trial <- df %>% 
  filter(trial_type == "change_12") %>%
  filter(trial == sample(unique(trial),size=1))

# plot trial data
ggplot(single.trial, aes(x,y,color=obj.color)) + 
  geom_point(aes(size=obj_identity)) +
  scale_colour_identity()+
  theme_bw() + 
  theme(panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "white")) +
  ggtitle(paste0("Trial #: ", single.trial$trial[1], " | ", 
                 single.trial$trial_type[1])) +
  facet_grid(. ~ change_trial)
