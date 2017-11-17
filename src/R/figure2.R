# Aggregate by FishID ***Short fire frequency and burned area*****------------------------------
library(ggthemes)
library(ggmap)
class_totals <- as.data.frame(fpa_wui) %>%
  group_by(fishid25k, class) %>%
  summarize(tot_fire = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire))

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

conus_ff <- left_join(fs25_df, fire_density, by = "fishid25k")

# now create the map
colourCount_ff = length(unique(bucket(conus_ff$n_den, 10)))
getPalette_ff = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                                 space = "Lab")

p100 <- conus_ff %>%
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

ggsave(file = "results/Gridded_PctFDen.eps", p100, width = 8, height = 5, dpi=1200) #saves g




eco_sum_bae <- as.data.frame(fpa_bae) %>%
  group_by(fishid25k, ignition, class, VLD, Wildlands, WUI) %>%
  summarize(t_cnt = n(),
            t_sum = sum(AREA_km2)) %>%
  ungroup()  %>%
  group_by(fishid25k, ignition, class) %>%
  summarise(vld2 = sum(VLD),
            wild2 = sum(Wildlands),
            wui2 = sum(WUI),
            f_cnt= sum(t_cnt),
            sum_fire = sum(t_sum)) %>%
  mutate(pct_burn = ifelse(class == "WUI", (sum_fire/wui2)*100,
                           ifelse(class == "VLD", (sum_fire/vld2)*100,
                                  ifelse(class == "Wildlands", (sum_fire/wild2)*100, 0))),
         pct_burn = ifelse(pct_burn >100, 100, pct_burn),
         pct_class = classify_pctbae(pct_burn),
         ptsz_n = classify_ptsize_breaks(f_cnt)) %>%
  select(fishid25k, ignition, class, f_cnt, sum_fire, pct_burn, pct_class, ptsz_n)

Gen_Sum <- wuw_eco_bae %>%
  group_by(fishid25k, class) %>%
  summarise(f_cnt= n()) %>%
  mutate(ptsz_n = classify_ptsize_breaks(f_cnt))

bae_Den <- wuw_eco_bae %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarize(t_sum = sum(AREA_km2)) %>%
  spread(IGNITION, t_sum) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         tot_fire = (n_Human + n_Lightning),
         n_den = ifelse(is.na((1-(n_Lightning/tot_fire))*100), 0, (1-(n_Lightning/tot_fire))*100)) %>%
  gather(., key = IGNITION, value = bae, -FishID_25k, -Class, -n_Human, -n_Lightning, -tot_fire, -n_den) %>%
  mutate(IGNITION = as.factor(IGNITION)) %>%
  left_join(., eco_sum_bae, by = c("FishID_25k", "Class", "IGNITION")) %>%
  select(FishID_25k, IGNITION, Class, pct_class, tot_fire, n_Human, n_Lightning, n_den) %>%
  left_join(., Gen_Sum, by = c("FishID_25k", "Class"))

Den_bae <- left_join(fs25_df, bae_Den, by = "FishID_25k")
GA_CONUS_bae <- left_join(fs25_df, eco_sum_bae, by = "FishID_25k")

# now create the map
colourCount_bae = length(unique(bucket(Den_bae$n_den, 10)))
getPalette_bae = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                                  space = "Lab")

p100_bae <- Den_bae %>%
  filter(IGNITION == "Human") %>%
  filter(n_den != "NA") %>%
  filter(Class != "VLD") %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
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
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

ManReds = brewer.pal(n = 9, "Reds")[3:10] #there are 9, I exluded the two lighter hues

p100 <- GA_CONUS_bae %>%
  filter(IGNITION == "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(Class != "VLD") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +
  scale_size_discrete(range = c(0.3,1.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p101 <- GA_CONUS_bae %>%
  filter(IGNITION != "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(Class != "VLD") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +
  scale_size_discrete(range = c(0.3,1.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  ggtitle('') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  facet_grid(Class ~ IGNITION, switch = "y")

p100a <- p100 + theme(legend.position="none")
p101a <- p101 + theme(legend.position="none")

grid.arrange(p100a, p101a, ncol =2)
g <- arrangeGrob(p100a, p101a, ncol = 2) #generates g
ggsave(file = "Gridded_WUI_BAE.png", g, width = 8, height = 5, dpi=1200) #saves g
legend <- g_legend(p100)
ggsave(file = "./legend/Gridded_WUI_BAE_Legend.png", legend, width = 2, height = 4.5, dpi=1200) #saves g
