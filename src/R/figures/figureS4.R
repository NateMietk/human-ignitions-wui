
maxseasons <- as.data.frame(fpa_wui) %>%
  group_by(fishid50k, ignition, class, seasons) %>%
  summarise(fire_freq = n()) %>%
  ungroup() %>%
  spread(seasons, fire_freq) %>%
  group_by(fishid50k, class, ignition) %>%
  mutate(Fall = ifelse(is.na(as.numeric(Fall)), 0, as.numeric(Fall)),
         Spring = ifelse(is.na(as.numeric(Spring)), 0, as.numeric(Spring)),
         Summer = ifelse(is.na(as.numeric(Summer)), 0, as.numeric(Summer)),
         Winter = ifelse(is.na(as.numeric(Winter)), 0, as.numeric(Winter)),
         fire_freq = Fall + Spring + Summer + Winter) %>%
  do(get_month_max(.)) 

maxseasons_full <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, ignition, class, seasons) %>%
  summarise(fire_freq = n()) %>%
  ungroup() %>%
  spread(seasons, fire_freq) %>%
  group_by(fishid50k, class, ignition) %>%
  mutate(Fall = ifelse(is.na(as.numeric(Fall)), 0, as.numeric(Fall)),
         Spring = ifelse(is.na(as.numeric(Spring)), 0, as.numeric(Spring)),
         Summer = ifelse(is.na(as.numeric(Summer)), 0, as.numeric(Summer)),
         Winter = ifelse(is.na(as.numeric(Winter)), 0, as.numeric(Winter)),
         fire_freq = Fall + Spring + Summer + Winter) %>%
  left_join(., maxseasons, by = c("fishid50k", "class", "ignition")) %>%
  mutate(ptsz_n = classify_ptsize_breaks(fire_freq),
         max_season = as.factor(max_season))

conus_maxseason <- left_join(fs50_df, maxseasons_full, by = "fishid50k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2)

p1 <- conus_maxseason %>%
  filter(!(is.na(ptsz_n))) %>%
  filter(class %in% c("Intermix WUI", 'Interface WUI', "VLD", "Wildlands")) %>%
  transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(class = factor(class, levels=c("Intermix WUI", 'Interface WUI', "VLD", "Wildlands"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = long, y = lat, colour = factor(max_season), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_color_manual(values = c("Winter" = "#1F77B4", 
                                "Spring" = "#2CA02C", 
                                "Summer" =  "#D62728", 
                                "Fall" = "#FF7F0E"), 
                     name="Max Season") + 
  scale_size_discrete(range = c(0.2, 0.9)) +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(class ~ ignition, switch ="y")

p1l <- p1 + theme(legend.position="none")

ggsave(file =  file.path(supplements_text_figs, "figureS4.tiff"), p1l, width = 7, height = 8, dpi=1200) #saves g

legend <- g_legend(p1) 
ggsave(file =  file.path(supplements_text_figs, "figureS4_legend.tiff"), 
       legend, width = 2, height = 4.5, dpi=1200) #saves g

system(paste0("aws s3 sync figs s3://earthlab-natem/human-ignitions-wui/figs"))
