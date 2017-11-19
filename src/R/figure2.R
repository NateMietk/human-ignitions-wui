# Aggregate by FishID ***Short fire frequency and burned area*****------------------------------
library(ggthemes)
library(ggmap)

class_totals <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, class) %>%
  summarize(tot_fire = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire))

bae_totals <- as.data.frame(fpa_bae) %>%
  group_by(fishid25k, class) %>%
  summarise(fire_freq = n(),
            tot_burn_area = sum(fire_size_km2)) %>%
  mutate(ptsz_n = classify_ptsize_breaks(fire_freq))

fire_density <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, ignition, class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(ignition, n_fire) %>%
  left_join(., class_totals, by = c("class", "fishid25k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_fire-n_Human),
         n_light_den = (tot_fire-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100)

wui_burned_area <- as.data.frame(fpa_bae) %>%
  group_by(fishid25k, ignition, class) %>%
  summarize(wui_area = sum(wui_area_km2)) %>%
  left_join(., wvw_area_fish, by = "fishid25k") %>%
  left_join(., fpa_wui_class_cause_fishnet25k, by = c("fishid25k", "ignition", "class")) %>%
  mutate(pct_burn = ifelse(class == "WUI", (wui_area/WUI)*100,
                           ifelse(class == "VLD", (wui_area/VLD)*100,
                                  ifelse(class == "Wildlands", (wui_area/Wildlands)*100, 0))),
         pct_burn = ifelse(pct_burn >100, 100, pct_burn),
         pct_class = classify_pctbae(pct_burn),
         ptsz_n = classify_ptsize_breaks(fire_freq)) %>%
  dplyr::select(fishid25k, ignition, class, wui_area, pct_burn, ptsz_n, pct_class, fire_freq)

burn_area_density <- as.data.frame(fpa_bae) %>%
  group_by(fishid25k, ignition, class) %>%
  summarize(burn_area = sum(fire_size_km2)) %>%
  left_join(., bae_totals, by = c("fishid25k", "class")) %>%
  spread(ignition, burn_area) %>%
  mutate(s_Human = ifelse(is.na(Human), 0, Human),
         s_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         tot_fire = (s_Human + s_Lightning),
         s_den = ifelse(is.na((1-(s_Lightning/tot_fire))*100), 0, (1-(s_Lightning/tot_fire))*100)) 

conus_ff <- left_join(fs25_df, fire_density, by = "fishid25k")

conus_wui_burned <- left_join(fs25_df, wui_burned_area, by = "fishid25k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2)

conus_burn_area <- left_join(fs25_df, burn_area_density, by = "fishid25k")  %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2)

# now create the map
colourCount_ff = length(unique(bucket(conus_ff$n_den, 10)))
getPalette_ff = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                                 space = "Lab")

p1 <- conus_ff %>%
  filter(n_den != "NA") %>%
  filter(class != "VLD") %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(class = factor(class, levels=c("WUI", "Wildlands"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = getPalette_ff(colourCount_ff), name="Percent") +
  scale_size_discrete(range = c(.3,0.9), name="Fire size (km2)") +
  ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ class, ncol=1, strip.position = 'left')

ManReds = brewer.pal(n = 9, "Reds")[2:10] #there are 9, I exluded the two lighter hues

p2 <- conus_wui_burned %>%
  filter(ignition == "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(pct_class != "NA") %>%
  filter(ptsz_n != "NA") %>%
  filter(class != "VLD") %>%
  transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", 
                                                   "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), 
               color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = long, y = lat, colour = factor(pct_class), size = ptsz_n)) +
  coord_equal() +
  scale_colour_manual(values = ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.2, 1.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(class ~ ignition, switch = "y")

ggsave(file = "results/Gridded_PctFDen.eps", p100, width = 8, height = 5, dpi=1200) #saves g
ggsave(file = "results/Gridded_PctFDen.tiff", p100, width = 8, height = 5, dpi=1200) #saves g



# now create the map
colourCount_bae = length(unique(bucket(conus_burn_area$s_den, 10)))
getPalette_bae = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                                  space = "Lab")

p100_bae <- conus_burn_area %>%
  filter(s_den != "NA") %>%
  filter(class != "VLD") %>%
  mutate(buckets = bucket(s_den, 10)) %>%
  transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(data = st_df, aes(group = group), 
               color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = factor(buckets), size = ptsz_n)) +
  coord_equal() +
  scale_colour_manual(values = getPalette_bae(colourCount_bae), name="Percent") +
  scale_size_discrete(range = c(.3,1.5), name="Class burned (%)") +
  theme_nothing(legend = TRUE) +
  ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ class, ncol=1, strip.position = 'left')


