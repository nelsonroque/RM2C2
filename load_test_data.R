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

library(tidyverse)
library(readr)
library(RM2C2)
# ----------------------------------
# SET PATHS
# ----------------------------------

#pc.path <- "~/Apps/"
pc.path <- "~/"
project.path <- paste0(pc.path,"Github/RM2C2/data/parsed/")

# ----------------------------------
# LOAD DATA
# ----------------------------------

stroop <- read_delim(paste0(project.path,
                                "gamedata_Color Naming Task_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

dot_memory <- read_delim(paste0(project.path,
                                "gamedata_Dot Memory_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

symbol_search <- read_delim(paste0(project.path,
                                   "gamedata_Symbol Search_2019_2_13_15_13.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

color_shapes <- read_delim(paste0(project.path,
                                   "gamedata_Color Shapes 2_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

color_speed <- read_delim(paste0(project.path,
                                  "gamedata_Color Speed_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

color_dots <- read_delim(paste0(project.path,
                                 "gamedata_Color Dots_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

ospan <- read_delim(paste0(project.path,
                            "gamedata_Letter Memory_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE, na='.')

sspan <- read_delim(paste0(project.path,
                                   "gamedata_Location Memory_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE, na='.')

rspan <- read_delim(paste0(project.path,
                                   "gamedata_Arrow Memory_2019_2_13_15_13.txt"), "|", escape_double = FALSE, trim_ws = TRUE, na='.')

go_nogo <- read_delim(paste0(project.path,
                             "gamedata_Go No Go_2019_2_13_15_13.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

change_detection <- read_delim(paste0(project.path,
                                      "gamedata_Change Detection_2019_2_13_15_13.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

shopping_list <- read_delim(paste0(project.path,
                                   "gamedata_Shopping List_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

quick_tapping <- read_delim(paste0(project.path,
                                   "gamedata_Quick Tapping_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

seq_tapping <- read_delim(paste0(project.path,
                                   "gamedata_Sequence Tapping_2019_2_24_18_35.txt"), "|", escape_double = FALSE, trim_ws = TRUE)

visual_wm <- read_delim(paste0(project.path,
                                 "gamedata_Visual Working Memory_2019_2_24_18_35.txt"), "|", 
                        escape_double = FALSE, trim_ws = TRUE, na='.')
