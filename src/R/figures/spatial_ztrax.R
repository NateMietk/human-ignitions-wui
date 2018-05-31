

fire_density <- bu_cleaned %>%
  group_by(fishid50k, ignition) %>%
  summarise(fcnt = n()) %>%
  ungroup() %>%
  filter(!is.na(ignition)) %>%
  spread(ignition, fcnt) %>%
  left_join(., fishnet_50k, by = "fishid50k") %>%
  as.data.frame() %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = ((n_Human+n_Lightning)-n_Human),
         n_light_den = ((n_Human+n_Lightning)-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100) %>%
  dplyr::select(fishid50k, n_den) %>%
  as_tibble()

bu_df <- bu_cleaned %>%
  dplyr::select(-geom) %>%
  filter(bu > 0) %>%
  group_by(fishid50k, ignition) %>%
  summarise(fcnt = n(),
            bu = sum(bu, na.rm = TRUE),
            rate = bu/(2016-1992),
            bu_per_fire = bu/fcnt) %>%
  mutate(bu_class = classify_bu(bu),
         rate_class = classify_bu(rate),
         bu_per_fire_class = classify_bu_per_fire(bu_per_fire),
         fcnt_class = classify_ptsize_breaks(fcnt)) %>%
  left_join(fs50_df, ., by = "fishid50k") %>%
  left_join(fire_density, ., by = "fishid50k") %>%
  na.omit()

p1 <- bu_df %>%
  transform(bu_class = factor(bu_class, levels = c("0 - 25", "25 - 250", "250 - 1000", "1000 - 10000", "10000 - 100000", "> 100000"))) %>%
  transform(fcnt_class = factor(fcnt_class, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(fcnt_class), size = bu_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(5, "RdYlBu"))) +
  scale_size_discrete(range = c(0.5, 3), name="# of homes \nthreatened") +
  theme_map() +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ ignition, ncol = 1)
ggsave(file = "results/figs/draft/home_threat_fcnt_ignition.pdf", p1, width = 6, height = 7,
       dpi=1200, scale = 4, units = "cm") #saves g

p1 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(bu_class = factor(bu_class, levels = c("0 - 25", "25 - 250", "250 - 1000", "1000 - 10000", "10000 - 100000", "> 100000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(1, 5), name="# of homes \nthreatened") +
  theme_map() +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white"))
ggsave(file = "results/figs/draft/home_threat_pct_ignition.pdf", p1, width = 10, height = 6,
       dpi=1200, scale = 4, units = "cm") #saves g

p2 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(bu_per_fire_class = factor(bu_per_fire_class, levels = c("0 - 10", "10 - 50", "50 - 250", "250 - 500", "> 500"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_per_fire_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(1, 5), name="Homes threatened \nper wildfire") +
  theme_map() +
  # ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white"))
ggsave(file = "results/figs/draft/home_per_fire_pct_ignition.pdf", p2, width = 10, height = 6,
       dpi=1200, scale = 4, units = "cm") #saves g

fire_density <- bu_cleaned %>%
  filter(class != 'Other') %>%
  mutate(class =  ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                          ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))) %>%
  group_by(fishid50k, ignition, class) %>%
  summarise(fcnt = n()) %>%
  ungroup() %>%
  filter(!is.na(ignition)) %>%
  spread(ignition, fcnt) %>%
  left_join(., fishnet_50k, by = "fishid50k") %>%
  as.data.frame() %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = ((n_Human+n_Lightning)-n_Human),
         n_light_den = ((n_Human+n_Lightning)-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100) %>%
  dplyr::select(fishid50k, class, n_den) %>%
  as_tibble()

bu_df <- bu_cleaned %>%
  filter(class != 'Other') %>%
  mutate(class =  ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                          ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))) %>%
  dplyr::select(-geom) %>%
  filter(bu > 0) %>%
  group_by(fishid50k, ignition, class) %>%
  summarise(fcnt = n(),
            bu = sum(bu, na.rm = TRUE),
            rate = bu/(2016-1992),
            bu_per_fire = bu/fcnt) %>%
  mutate(bu_class = classify_bu(bu),
         rate_class = classify_bu(rate),
         bu_per_fire_class = classify_bu_per_fire(bu_per_fire)) %>%
  left_join(fs50_df, ., by = "fishid50k") %>%
  left_join(fire_density, ., by = c('class', "fishid50k")) %>%
  na.omit()

p1 <- bu_df %>%
  filter(class == 'WUI' | class == 'Wildlands') %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(class = factor(class, levels=c('Urban', "WUI", "VLD", "Wildlands"))) %>%
  transform(bu_class = factor(bu_class, levels = c("0 - 25", "25 - 250", "250 - 1000", "1000 - 10000", "10000 - 100000", "> 100000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(.25, 3), name="# of homes \nthreatened") +
  theme_map() +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class, ncol = 1)
ggsave(file = "results/figs/draft/wui_home_threat_pct_ignition.pdf", p1, width = 6, height = 7,
       dpi=1200, scale = 4, units = "cm") #saves g

p2 <- bu_df %>%
  filter(class == 'WUI' | class == 'Wildlands') %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(class = factor(class, levels=c('Urban', "WUI", "VLD", "Wildlands"))) %>%
  transform(bu_per_fire_class = factor(bu_per_fire_class, levels = c("0 - 10", "10 - 50", "50 - 250", "250 - 500", "> 500"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_per_fire_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(.25, 3), name="Homes threatened \nper wildfire") +
  theme_map() +
  # ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class, ncol = 1)
ggsave(file = "results/figs/draft/wui_home_per_fire_pct_ignition.pdf", p2, width = 6, height = 7,
       dpi=1200, scale = 4, units = "cm") #saves g


p1 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(class = factor(class, levels=c('Urban', "WUI", "VLD", "Wildlands"))) %>%
  transform(bu_class = factor(bu_class, levels = c("0 - 25", "25 - 250", "250 - 1000", "1000 - 10000", "10000 - 100000", "> 100000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(.25, 3), name="# of homes \nthreatened") +
  theme_map() +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class, ncol = 2)
ggsave(file = "results/figs/draft/allwui_home_threat_pct_ignition.pdf", p1, width = 10, height = 7,
       dpi=1200, scale = 4, units = "cm") #saves g

p2 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(class = factor(class, levels=c('Urban', "WUI", "VLD", "Wildlands"))) %>%
  transform(bu_per_fire_class = factor(bu_per_fire_class, levels = c("0 - 10", "10 - 50", "50 - 250", "250 - 500", "> 500"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_per_fire_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(.25, 3), name="Homes threatened \nper wildfire") +
  theme_map() +
  # ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class, ncol = 2)
ggsave(file = "results/figs/draft/allwui_home_per_fire_pct_ignition.pdf", p2, width = 10, height = 7,
       dpi=1200, scale = 4, units = "cm") #saves g
