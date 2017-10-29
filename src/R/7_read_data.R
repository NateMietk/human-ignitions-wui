source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

# ICS 209 database from 2001-2013
wui_209 <- hex_grid_50k %>%
  st_join(., st_read("data/ics209/spatial/ics209_wui_conus.gpkg"), join = st_intersects)

wuw_eco_ICS <- wui_209 %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

eco_sum_ICS <- wuw_eco_ICS %>%
  group_by(hex50k_id, Class, cause) %>%
  summarize(f_cnt = n(),
            sum_costs = sum(costs),
            avg_costs = mean(costs),
            fire_size_km2 = sum(area_km2),
            sum_homethreat = sum(as.numeric(home.threat))) %>%
  ungroup()  %>%
  mutate(ptsz_sc = classify_suppresscosts(sum_costs),
         ptsz_s = classify_fire_size(fire_size_km2),
         ptsz_t = classify_homesthreat(sum_homethreat))

hx50_df <- fortify(as(hex_grid_50k, "Spatial"), region = "hex50k_id") %>%
  mutate(hex50k_id = as.numeric(id))

wui_209_p <- left_join(hx50_df, eco_sum_ICS, by = "hex50k_id")

ManReds = brewer.pal(n = 9, "Reds")[2:10] #there are 9, I exluded the two lighter hues

wui_209_p %>%
  filter(!(cause %in% c("NA","Unk"))) %>%
  filter(!(Class %in% c("VLD", "NA"))) %>%
  filter(sum_costs != "NA") %>%
  filter(!(is.na(ptsz_sc))) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                             "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot() +
  geom_sf(data = conus, color='black', fill = "gray99", size = .50) +
  geom_polygon(aes(x = long, y = lat, colour = as.factor(ptsz_sc), group = hex50k_id,
                 fill = as.factor(ptsz_sc), size = ptsz_t), position = "identity") +
  scale_fill_manual(values=ManReds, name = "Costs") +  
  scale_color_manual(values=ManReds, name = "Costs") +  
  scale_size_discrete(range = c(0.1, .5), name="Total homes threatened") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        panel.grid.major = element_line(color = "white"),
        strip.background=element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ cause, switch = "y")

