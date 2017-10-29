source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

# ICS 209 database from 2001-2013
wui_209 <- st_read("data/ics209/spatial/ics209_wui_conus.gpkg") %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

names(wui_209) %<>% tolower

# ICS 209 database from 2001 - 2015
fpa_wui <- st_read("data/fpa-fod/fpa_wui_conus.gpkg") %>%
  mutate(Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(DISCOVERY_YEAR < 2000)

names(fpa_wui) %<>% tolower

fpa_209 <- hex_grid_25k %>%
  st_join(., fpa_wui, join = st_intersects, left = TRUE) 
  
fpa_tots <- fpa_209 %>%
  group_by(hex25k_id.x, class.x) %>%
  summarize(tot_fire = n(),
            sum_totfire = sum(fire_size_km2),
            avg_totfire = mean(fire_size_km2)) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire),
         ptsz_s = classify_fire_size(sum_totfire))
  
fpa_209_sum <- fpa_209 %>%
  group_by(hex25k_id.x, class.x, ignition) %>%
  summarize(n_fire = n(),
            sum_costs = sum(costs),
            avg_costs = mean(costs),
            fire_size_km2 = mean(fire_size_km2),
            sum_homethreat = sum(as.numeric(home.threat))) %>%
  ungroup()  %>%
  spread(ignition, n_fire) %>%
  left_join(., fpa_tots, by = c("class.x", "hex25k_id.x")) %>%
  mutate(ff_h = ifelse(is.na(Human), 0, Human), 
         ff_l = ifelse(is.na(Lightning), 0, Lightning),
         fd_h = (tot_fire-ff_h),
         fd_l = (tot_fire-ff_l),
         tot_den = (1-(fd_h/(fd_h+fd_l)))*100) %>%
  mutate(ptsz_sc = classify_suppresscosts(sum_costs),
         ptsz_s = classify_fire_size(fire_size_km2),
         ptsz_t = classify_homesthreat(sum_homethreat)) 

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

