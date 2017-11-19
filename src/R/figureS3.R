
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
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(class ~ ignition, switch ="y")

p1l <- p1 + theme(legend.position="none")

ggsave(file = "figs/figs_main/drafts/figureS3.eps", p1l, width = 7, height = 8, dpi=1200) #saves g
ggsave(file = "figs/figs_main/drafts/figureS3.tiff", p1l, width = 7, height = 8, dpi=1200) #saves g

legend <- g_legend(p1) 
ggsave(file = "figs/figs_main/drafts/figureS3_legend.eps", 
       legend, width = 2, height = 4.5, dpi=1200) #saves g

