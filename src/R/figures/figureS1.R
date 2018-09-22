
ics_totals <- as.data.frame(wui_209) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(fishid25k, class) %>%
  summarise(tot_costs = sum(costs)) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_suppresscosts(tot_costs)) %>%
  filter(ptsz_n != 0)

bu_totals <- as.data.frame(bu_complete_cleaned) %>%
  mutate_if(is.character, as.factor) %>%
  filter(built_class == 'Residential' & discovery_year != 0) %>%
  group_by(fishid25k, class) %>%
  summarise(tot_bu = sum(build_up_count_no_zero_0)) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_bu(tot_bu))

bae_totals <- as.data.frame(fpa_bae_wui) %>%
  setNames(tolower(names(.))) %>%
  group_by(fishid25k, class) %>%
  summarise(fire_freq = n(),
            tot_burn_area = sum(fire_size_km2)) %>%
  mutate(ptsz_n = classify_ptsize_breaks(fire_freq),
         frsz_cl = classify_raw_fire_size(tot_burn_area))

class_totals <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, class) %>%
  summarise(tot_fire = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire))

wui_fish25k_sum <- ungroup(read_rds(file.path(wui_out, "wui_fish25k_sum.rds"))) %>%
  filter(year == 2010) %>%
  dplyr::select(-year)

fire_density <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, ignition, class) %>%
  summarise(n_fire = n()) %>%
  ungroup() %>%
  spread(ignition, n_fire) %>%
  left_join(., class_totals, by = c("class", "fishid25k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_fire-n_Human),
         n_light_den = (tot_fire-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100)

bu_density <- as.data.frame(bu_complete_cleaned) %>%
  filter(built_class == 'Residential' & discovery_year != 0) %>%
  group_by(fishid25k, ignition, class) %>%
  summarise(built_count = sum(build_up_count_no_zero_0)) %>%
  ungroup() %>%
  spread(ignition, built_count) %>%
  left_join(., bu_totals, by = c("class", "fishid25k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_bu-n_Human),
         n_light_den = (tot_bu-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100,
         n_den = ifelse(is.na(n_den) | is.nan(n_den), 0, n_den))

ics_density <- as.data.frame(wui_209) %>%
  group_by(fishid25k, cause, class) %>%
  summarise(costs = sum(costs)) %>%
  ungroup() %>%
  spread(cause, costs) %>%
  left_join(., ics_totals, by = c("class", "fishid25k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_costs-n_Human),
         n_light_den = (tot_costs-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100,
         n_den = ifelse(is.na(n_den) | is.nan(n_den), 0, n_den))

burn_area_density <- as.data.frame(fpa_bae_wui) %>%
  setNames(tolower(names(.))) %>%
  left_join(., wui_fish25k_sum, by = c("fishid25k", "class")) %>%
  group_by(fishid25k, ignition, class) %>%
  summarise(wui_area_km2 = sum(wui_area_km2),
            total_fishid25k_area = max(total_fishid25k_area)) %>%
  spread(ignition, wui_area_km2) %>%
  left_join(., bae_totals, by = c("class", "fishid25k")) %>%
  mutate(s_Human = ifelse(is.na(Human), 0, Human),
         s_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         tot_fire = (s_Human + s_Lightning),
         s_den = ifelse(is.na((1-(s_Lightning/tot_fire))*100), 0, (1-(s_Lightning/tot_fire))*100),
         pct_class_ba_human = s_Human/total_fishid25k_area*100,
         pct_class_human = classify_pctbae(pct_class_ba_human),
         pct_class_ba_lightning = s_Lightning/total_fishid25k_area*100,
         pct_class_lightning = classify_pctbae(pct_class_ba_lightning)) 

conus_ff <- left_join(fs25_df, fire_density, by = "fishid25k") %>%
  na.omit()

conus_burn_area <- left_join(fs25_df, burn_area_density, by = "fishid25k")  %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 

conus_bu <- left_join(fs25_df, bu_density, by = "fishid25k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 

conus_ics <- left_join(fs25_df, ics_density, by = "fishid25k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 
  
p1 <- conus_bu %>%
  na.omit() %>%
  filter(class %in% c('Intermix WUI', 'Interface WUI', "VLD", 'Wildlands')) %>%
  transform(class = factor(class, levels=c('Interface WUI', 'Intermix WUI', "VLD", 'Wildlands'))) %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("0 - 25", "25 - 250", "250 - 1000", "1000 - 10000", "> 10000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), 
               color='black', fill = "gray97", size = .25) +
  geom_point(aes(x = long, y = lat, colour = factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11,"RdYlBu"))) +
  scale_size_discrete(range = c(0.1, 0.5), name="Homes threatened") +
  theme_nothing(legend = TRUE) +
  # ggtitle('(C) Homes threatened') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(~class)

p2 <- conus_ff %>%
  filter(class %in% c('Intermix WUI', 'Interface WUI', "VLD", 'Wildlands')) %>%
  transform(class = factor(class, levels=c('Interface WUI', 'Intermix WUI', "VLD", 'Wildlands'))) %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black', fill = "gray97", size = .25)+
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(10,"RdYlBu"))) +
  scale_size_discrete(range = c(0.1, 0.5), name="Fire size (km2)") +
  theme_nothing(legend = TRUE) +
  # ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(~class)

p3 <- conus_burn_area %>%
  na.omit() %>%
  filter(class %in% c('Intermix WUI', 'Interface WUI', "VLD", 'Wildlands')) %>%
  transform(class = factor(class, levels=c('Interface WUI', 'Intermix WUI', "VLD", 'Wildlands'))) %>%
  transform(pct_class_human = factor(pct_class_human, levels=c("< 1", "1 - 10", "10 - 20", 
                                                   "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(frsz_cl = factor(frsz_cl, levels=c("0-4", "4-100", "100-400", "400-1000", ">1000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), 
               color='black', fill = "gray97", size = .25) +
  geom_point(aes(x = long, y = lat, colour = factor(pct_class_human), size = frsz_cl), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(7,"RdYlBu"))) +
  scale_size_discrete(range = c(0.1, 0.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  # ggtitle('(B) Percent class burned')+
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(~class)

p4 <- conus_burn_area %>%
  na.omit() %>%
  filter(class %in% c('Intermix WUI', 'Interface WUI', "VLD", 'Wildlands')) %>%
  transform(class = factor(class, levels=c('Interface WUI', 'Intermix WUI', "VLD", 'Wildlands'))) %>%
  transform(pct_class_lightning = factor(pct_class_lightning, levels=c("< 1", "1 - 10", "10 - 20", 
                                                               "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(frsz_cl = factor(frsz_cl, levels=c("0-4", "4-100", "100-400", "400-1000", ">1000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), 
               color='black', fill = "gray97", size = .25) +
  geom_point(aes(x = long, y = lat, colour = factor(pct_class_lightning), size = frsz_cl), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(7,"RdYlBu"))) +
  scale_size_discrete(range = c(0.1, 0.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  # ggtitle('(B) Percent class burned')+
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(~class)

p1l <- p1 + theme(legend.position="none")
p2l <- p2 + theme(legend.position="none")
p3l <- p3 + theme(legend.position="none")
p4l <- p4 + theme(legend.position="none")

grid.arrange(p1l, p2l, p3l, p4l, ncol = 1)
g <- arrangeGrob(p1l, p2l, p3l, p4l, ncol = 1) #generates g

ggsave(file = file.path(supplements_text_figs, "figureS1.tiff"), g, width = 12, height = 9, dpi=1200) #saves g

legend <- g_legend(p1) 
ggsave(file = file.path(supplements_text_figs, "figureS1a_legend.tiff"), 
       legend, width = 2, height = 4.5, dpi=1200) #saves g
legend <- g_legend(p2) 
ggsave(file = file.path(supplements_text_figs, "figureS1b_legend.tiff"), 
       legend, width = 2, height = 4.5, dpi=1200) #saves g
legend <- g_legend(p3) 
ggsave(file = file.path(supplements_text_figs, "figureS1c_legend.tiff"), 
       legend, width = 2, height = 4.5, dpi=1200) #saves g
legend <- g_legend(p4) 
ggsave(file = file.path(supplements_text_figs, "figureS1d_legend.tiff"), 
       legend, width = 2, height = 4.5, dpi=1200) #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))

