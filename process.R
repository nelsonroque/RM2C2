rm(a); rm(b);
a <- score_symbol_search(symbol_search)
b <- summary_symbol_search(a,c("user_id","trial_type"))

rm(a); rm(b);
a <- score_color_speed(color_speed)
b <- summary_color_speed(a,"user_id")

rm(a); rm(b);
a <- score_dot_memory(dot_memory)
b <- summary_dot_memory(a,"user_id")

rm(a); rm(b);
a <- score_color_shapes(color_shapes)
b <- summary_color_shapes(a,"user_id")

rm(a); rm(b);
a <- score_go_nogo(go_nogo)
b <- summary_go_nogo(a, "user_id")

rm(a); rm(b);
a <- score_change_detection(change_detection)
b <- summary_change_detection(a, c("user_id","square_num"))

rm(a); rm(b);
a <- score_shopping_list(shopping_list)
b <- summary_shopping_list(a, "user_id")


rm(a); rm(b);
a <- score_stroop(stroop)
b <- summary_stroop(a, "user_id")

rm(a); rm(b);
a <- score_ospan(ospan)
b <- summary_ospan(a, "user_id")


rm(a); rm(b);
a <- score_sspan(sspan)
b <- summary_sspan(a, "user_id")


rm(a); rm(b);
a <- score_rspan(rspan)
b <- summary_rspan(a, "user_id")


rm(a); rm(b);
a <- restructure_tapping_data(quick_tapping)
b <- score_quick_tapping(a, t_lag=3)
c <- summary_quick_tapping(b, c("user_id","trial_num"))

ggplot(b, aes(tap_number,velocity)) + 
  geom_point() + 
  facet_grid(trial_num~.)


ggplot(b, aes(tap_number,acceleration)) + 
  geom_point() + 
  facet_grid(trial_num~.)

  rm(a); rm(b); rm(c);
a <- restructure_tapping_data(seq_tapping, c(1:24), c(25))
b <- score_seq_tapping(a, t_lag=3)
c <- summary_seq_tapping(b, group_var=c("user_id"), nontap_cols=c(1:32))

rm(a); rm(b); rm(c);

zz <- read_ambcog("C:/Users/nur375/Box/EAS data files for AAIC submission/Raw EMA files (SAS)/color_dots.sas7bdat")

a <- score_color_dots(zz,threshold=75)
b <- summary_color_dots(a,"id")

write.csv(a,"~/Github/tmp/cd.csv",row.names = F)

visual_wm <- read_delim(paste0("C:/Users/nur375/Desktop/10101/out/out_2019_3_1_15_16/gamedata_Visual Working Memory_2019_3_1_15_16.txt"), "|", 
                        escape_double = FALSE, trim_ws = TRUE, na='.')
a <- subset(visual_wm, game_uuid == "e0a8cdde-304f-4744-b4a1-25540f0d4ae4")


b <- score_visual_wm(a) %>%
  group_by(block_num,trialset_num,trial_num) %>%
  mutate(row.n = row_number())


c <- summary_visual_wm(b, group_var="user_id")