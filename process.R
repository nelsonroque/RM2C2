# ===================================================================================
# PENDING ITEMS
# ===================================================================================

# -------------------------------------------------
# WISHLIST
# -------------------------------------------------
# - flagging for things, based on app events, device motion events (calls, texts, other)
# - classify response order for span tasks

# -------------------------------------------------
# BY TASK
# -------------------------------------------------
# - Visual working memory: revise summary
# - Motor Sequencing: revise summary

# ----------------------------------
# LOAD LIBS
# ----------------------------------

#library(tidyverse)
#library(readr)
devtools::install_github("nelsonroque/RM2C2", force=T)
library(RM2C2)
#library(stringr)
# ----------------------------------
# SET PATHS
# ----------------------------------
#usr.path <- "~/"
#usr.path <- "C:/Users/nar09/"
usr.path <- "C:/Users/nur375/"

pc.path <- paste0(usr.path,"Box/Projects/status/current/RM2C2/")
project.path <- paste0(pc.path,"data/parsed/")

# ----------------------------------
# LOAD DATA
# ----------------------------------

stroop <- read_ambcog(paste0(project.path,
                            "gamedata_Color Naming Task_2019_2_24_18_35.txt"))

dot_memory <- read_ambcog(paste0(project.path,
                                "gamedata_Dot Memory_2019_2_24_18_35.txt"))

symbol_search <- read_ambcog(paste0(project.path,
                                   "gamedata_Symbol Search_2019_2_13_15_13.txt"))

color_shapes <- read_ambcog(paste0(project.path,
                                  "gamedata_Color Shapes 2_2019_2_24_18_35.txt"))

color_speed <- read_ambcog(paste0(project.path,
                                 "gamedata_Color Speed_2019_2_24_18_35.txt"))

color_dots <- read_ambcog(paste0(project.path,
                                "gamedata_Color Dots_2019_2_24_18_35.txt"))

ospan <- read_ambcog(paste0(project.path,
                           "gamedata_Letter Memory_2019_2_24_18_35.txt"))

sspan <- read_ambcog(paste0(project.path,
                           "gamedata_Location Memory_2019_2_24_18_35.txt"))

rspan <- read_ambcog(paste0(project.path,
                           "gamedata_Arrow Memory_2019_2_13_15_13.txt"))

go_nogo <- read_ambcog(paste0(project.path,
                             "gamedata_Go No Go_2019_2_13_15_13.txt"))

na = "."
change_detection <- read_ambcog(filepath=paste0(project.path,
                                      "gamedata_Change Detection_2019_2_13_15_13.txt"), na=na)

shopping_list <- read_ambcog(paste0(project.path,
                                   "gamedata_Shopping List_2019_2_24_18_35.txt"))

quick_tapping <- read_ambcog(paste0(project.path,
                                   "gamedata_Quick Tapping_2019_2_24_18_35.txt"))

seq_tapping <- read_ambcog(paste0(project.path,
                                 "gamedata_Sequence Tapping_2019_2_24_18_35.txt"))

visual_wm <- read_ambcog(paste0(project.path,
                               "gamedata_Visual Working Memory_2019_2_24_18_35.txt"))

# --------------------------------------------------------------------------
a <- score_symbol_search(symbol_search)
b <- summary_symbol_search(a,c("user_id","trial_type"))
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_color_speed(color_speed)
b <- summary_color_speed(a,"user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_dot_memory(dot_memory)
b <- summary_dot_memory(a,"user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_color_shapes(color_shapes)
b <- summary_color_shapes(a,"user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_go_nogo(go_nogo)
b <- summary_go_nogo(a, "user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
change_detection <- read_csv("C:/Users/nur375/Box/Projects/status/current/RM2C2/data/parsed/gamedata_Change Detection_2019_3_11_10_54.txt")
a <- score_change_detection(change_detection)
b <- summary_change_detection(a, c("user_id","session","square_num"))
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_shopping_list(shopping_list)
b <- summary_shopping_list(a, "user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_stroop(stroop)
b <- summary_stroop(a, "user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_span(ospan)
b <- summary_ospan(a, "user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_span(sspan)
b <- summary_sspan(a, "user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_span(rspan)
b <- summary_rspan(a, "user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);

quick_tapping = read_delim("C:/Users/nur375/Box/Projects/status/current/RM2C2/data/parsed/gamedata_Quick Tapping_2019_2_24_18_35.txt", 
                           "|", escape_double = FALSE, trim_ws = TRUE)
a <- restructure_tapping_data(quick_tapping)
b <- score_quick_tapping(a, t_lag=3)
c <- summary_quick_tapping(b, c("user_id","trial_num"))

# ///////////////////////////////////////

ggplot(b, aes(tap_number,velocity)) + 
  geom_point() + 
  facet_grid(trial_num~.)
ggplot(b, aes(tap_number,acceleration)) + 
  geom_point() + 
  facet_grid(trial_num~.)

# --------------------------------------------------------------------------
rm(a); rm(b); rm(c);
a <- restructure_tapping_data(seq_tapping, c(1:24), c(25))
b <- score_seq_tapping(a, t_lag=3)
c <- summary_seq_tapping(b, group_var=c("user_id"), nontap_cols=c(1:32))
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_color_dots(color_dots,threshold=75)
b <- summary_color_dots(a,"user_id")
# --------------------------------------------------------------------------
rm(a); rm(b);
a <- score_visual_wm(visual_wm)
b <- summary_visual_wm(a, group_var="user_id")
