eco_sum <- as.data.frame(fpa_wui) %>%
  filter(seasons == "Summer" | seasons == "Spring") %>%
  group_by(fishid25k, class, seasons) %>%
  summarize(fire_freq = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(fire_freq))

seasonal_freq <- as.data.frame(fpa_wui) %>%
  filter(seasons == "Summer" | seasons == "Spring") %>%
  group_by(fishid25k, ignition, class, seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(ignition, n_fire) %>%
  left_join(., eco_sum, by = c("class", "fishid25k", "seasons")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human), 
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (fire_freq-n_Human),
         n_light_den = (fire_freq-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100) 

maxseasons <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, ignition, class, seasons) %>%
  summarize(fire_freq = n()) %>%
  ungroup() %>%
  spread(seasons, fire_freq) %>%
  group_by(fishid25k, class, ignition) %>%
  mutate(Fall = ifelse(is.na(as.numeric(Fall)), 0, as.numeric(Fall)),
         Spring = ifelse(is.na(as.numeric(Spring)), 0, as.numeric(Spring)),
         Summer = ifelse(is.na(as.numeric(Summer)), 0, as.numeric(Summer)),
         Winter = ifelse(is.na(as.numeric(Winter)), 0, as.numeric(Winter)),
         fire_freq = Fall + Spring + Summer + Winter) %>%
  do(get_month_max(.)) 

maxseasons_full <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, ignition, class, seasons) %>%
  summarize(fire_freq = n()) %>%
  ungroup() %>%
  spread(seasons, fire_freq) %>%
  group_by(fishid25k, class, ignition) %>%
  mutate(Fall = ifelse(is.na(as.numeric(Fall)), 0, as.numeric(Fall)),
         Spring = ifelse(is.na(as.numeric(Spring)), 0, as.numeric(Spring)),
         Summer = ifelse(is.na(as.numeric(Summer)), 0, as.numeric(Summer)),
         Winter = ifelse(is.na(as.numeric(Winter)), 0, as.numeric(Winter)),
         fire_freq = Fall + Spring + Summer + Winter) %>%
  left_join(., maxseasons, by = c("fishid25k", "class", "ignition")) %>%
  mutate(ptsz_n = classify_ptsize_breaks(fire_freq),
         max_season = as.factor(max_season))

conus_maxseason <- left_join(fs25_df, maxseasons_full, by = "fishid25k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) %>%
  na.omit()
# 
# #For Tableau blue
# rgb(31,119,180, maxColorValue = 255)
# #For Tableau orange
# rgb(255,127,14, maxColorValue = 255)
# #For Tableau red
# rgb(214,39,40, maxColorValue = 255)
# #For Tableau green
# rgb(44,160,44, maxColorValue = 255)

p1 <- conus_maxseason %>%
  filter(!(is.na(ptsz_n))) %>%
  transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(data = st_df, aes(group = group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(max_season), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("Winter" = "#1F77B4", 
                                "Spring" = "#2CA02C", 
                                "Summer" =  "#D62728", 
                                "Fall" = "#FF7F0E"), 
                     name="Max Season") + 
  scale_size_discrete(range = c(0.2, 1)) +
  coord_equal() + 
  theme_nothing(legend = FALSE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(class ~ ignition, switch ="y")

ggsave(file = "figs/figs_main/drafts/figureS3.eps", p1, width = 7, height = 8, dpi=1200) #saves g
ggsave(file = "figs/figs_main/drafts/figureS3.tiff", p1, width = 7, height = 8, dpi=1200) #saves g

