
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
