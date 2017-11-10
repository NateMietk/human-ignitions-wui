hex_fpa <- hex_grid_25k %>%
  st_join(., fpa_wui, join = st_intersects, left = TRUE)

fpa_tots <- hex_fpa %>%
  group_by(hex25k, class) %>%
  summarize(tot_fire = n(),
            sum_totfire = sum(fire_size_km2),
            avg_totfire = mean(fire_size_km2)) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire),
         ptsz_s = classify_fire_size(sum_totfire))

fpa_209_sum <- fpa_209 %>%
  group_by(hex25k, class, ignition) %>%
  summarize(n_fire = n(),
            fire_size_km2 = mean(fire_size_km2)) %>%
  ungroup()  %>%
  spread(ignition, n_fire) %>%
  left_join(., fpa_tots, by = c("class", "hex25k")) %>%
  mutate(ff_h = ifelse(is.na(Human), 0, Human),
         ff_l = ifelse(is.na(Lightning), 0, Lightning),
         fd_h = (tot_fire-ff_h),
         fd_l = (tot_fire-ff_l),
         tot_den = (1-(fd_h/(fd_h+fd_l)))*100) %>%
  mutate(pptsz_s = classify_fire_size(fire_size_km2))

hx25_df <- fortify(as(hex_grid_25k, "Spatial"), region = "hex25k_id") %>%
  mutate(hex25k_id = as.numeric(id))

wui_209_p <- left_join(hx25_df, fpa_209_sum, by = "hex25k_id")

ManReds = brewer.pal(n = 9, "Reds")[2:10] #there are 9, I exluded the two lighter hues
colourCount_ff = length(unique(bucket(wui_209_p$tot_den, 10)))
getPalette_ff = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                                 space = "Lab")

wui_209_p %>%
  filter(tot_den != "NA") %>%
  filter(!(class %in% c("VLD", "NA"))) %>%
  filter(tot_den >= 1) %>%
  mutate(buckets = bucket(tot_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_polygon(aes(x = long, y = lat, group = hex25k_id,
                   color = as.factor(buckets), size = as.factor(ptsz_n)),
               stat = "identity", inherit.aes = FALSE) +
  coord_equal() +
  scale_colour_manual(values = getPalette_ff(colourCount_ff), name="Percent") +
  scale_size_discrete(range = c(.3,.9), name="Fire size (km2)") +
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

wui_209_p %>%
  filter(!(cause %in% c("NA","Unk"))) %>%
  filter(!(Class %in% c("VLD", "NA"))) %>%
  filter(!(is.na(sum_costs))) %>%
  filter(!(is.na(ptsz_sc))) %>%
  filter(!(is.na(ptsz_t))) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 25,000", "25,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 250", "250 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot() +
  geom_polygon(data=states, aes(x = long, y = lat, group = group), color = 'black', fill = "gray99", size = .1)+
  geom_polygon(aes(x = long, y = lat, group = hex25k_id,
                   color = as.factor(ptsz_sc), fill = as.factor(ptsz_sc)),
               stat = "identity", inherit.aes = FALSE) +
  scale_fill_manual(values = ManReds, name = "Costs") +
  scale_color_manual(values=ManReds, name = "Costs") +
  theme_nothing(legend = FALSE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        panel.grid.major = element_line(color = "white"),
        strip.background=element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ cause, switch = "y")


wuw_eco_ICS %>%
  filter(!(cause %in% c("NA","Unk"))) %>%
  filter(!(Class %in% c("NA"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  filter(!(is.na(costs))) %>%
  ggplot(aes(x = costs, y = home.threat, color = cause)) +
  geom_smooth( method.args = list(family = "poisson")) +
  theme_pub() +
  facet_grid( ~ Class, switch = "y")

wuw_eco_ICS %>%
  filter(!(cause %in% c("NA","Unk"))) %>%
  filter(!(Class %in% c("NA"))) %>%
  filter(!(is.na(area_km2))) %>%
  ggplot(aes(x = costs, y = area_km2, color = cause)) +
  geom_smooth( method.args = list(family = "poisson")) +
  geom_hline(aes(yintercept = 4), show_guide=TRUE) +
  geom_hline(aes(yintercept = 250), show_guide=TRUE) +
  geom_hline(aes(yintercept = 1000), show_guide=TRUE) +
  theme_pub() +
  facet_grid( ~ Class, switch = "y")



pmod <- glmer(area_km2 ~
                costs*home.destroyed+
                (1|cause/Class),
              data = wuw_eco_ICS,
              family = "poisson")


## Set up a raster "template" to use in rasterize()
fpa_wui_0115 <- fpa_wui %>%
  filter(DISCOVERY_YEAR > 2000)

fpa_wui_hex <- st_as_sf(hex_grid_c) %>%
  mutate(hex25k_id = row_number()) %>%
  st_intersection(fpa_wui_0115, .)

eco_sum <- fpa_wui_hex %>%
  group_by(hex25k_id, Class) %>%
  summarize(tot_fire = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire))

fpa_wui_cause <- fpa_wui_hex %>%
  group_by(hex25k_id, IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, n_fire) %>%
  st_join(., eco_sum, join = st_intersects, suffix = c("Class", "hex25k_id")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_fire-n_Human),
         n_light_den = (tot_fire-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100)

# now create the map
colourCount_ff = length(unique(bucket(fpa_wui_cause$s_den, 10)))
getPalette_ff = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                                 space = "Lab")

p100 <- fpa_wui_cause %>%
  filter(n_den != "NA") %>%
  filter(ClassClass != "VLD") %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(ClassClass = factor(ClassClass, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(.) +
  geom_sf(data=conus, aes(fill = group), color='black', fill = "gray99", size = .25) +
  geom_sf(aes(colour = factor(buckets)), inherit.aes = FALSE, geom = "hex") +
  coord_equal() +
  scale_colour_manual(values = getPalette_ff(colourCount_ff), name="Percent") +
  scale_size_discrete(range = c(.3,.9), name="Fire size (km2)") +
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

plot(conus[1], col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid_c, border = "orange", add = TRUE)
box()


# Try to summarize distance from WUI using hexagonal
hex_grid_b <- make_grid(as(conus, "Spatial"), type = "hexagonal",
                        cell_area = 1000000000000, clip = TRUE)
plot(conus[1], col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid_b, border = "orange", add = TRUE)
box()
