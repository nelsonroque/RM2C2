ss <- symbol_search.f %>% mutate(dt = lubridate::date(time),
                                 wk = lubridate::wday(time, label=T)) %>%
  filter(response_time < 2500)

ss.tid <- ss %>% 
  group_by(user_id, dt) %>%
  mutate(trial_in_day = row_number()) %

# weekday plot
ggplot(ss, aes(wk, response_time)) + geom_point()

#
ggplot(ss %>% filter(user_id %in% c(11)) %>% filter(wk == "Sun"), 
       aes(response_time, wk)) + 
  geom_point()
       
# time x trials (in one day)
ggplot(ss.tid %>% filter(user_id %in% c(11)), 
       aes(trial_in_day, response_time, color=factor(session))) + 
  geom_point() +
  facet_grid(. ~ wk)

# day in study
# EARLIEST
# STUDY DAY 1
# STUDY PARTICIPATION DAY 1