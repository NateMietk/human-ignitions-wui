

# ICS 209 database from 2001-2013
wui_209 <- st_read("data/ics209/spatial/ics209_wui_conus.gpkg") %>%
  as.data.frame()

wuw_eco_ICS <- wui_209 %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

eco_sum_ICS <- wuw_eco_ICS %>%
  group_by(FishID25k, Class, cause) %>%
  summarize(f_cnt = n(),
            sum_costs = sum(costs),
            avg_costs = mean(costs),
            fire_size_km2 = sum(area_km2),
            sum_homethreat = sum(as.numeric(home.threat))) %>%
  ungroup()  %>%
  mutate(ptsz_sc = classify_suppresscosts(sum_costs),
         ptsz_s = classify_fire_size(fire_size_km2),
         ptsz_t = classify_homesthreat(sum_homethreat))

fs25_df <- fortify(as(fishnet_25k, "Spatial"), region = as.integer("id")) %>%
  left_join(., as.data.frame(fishnet_25k), by = "id")

GA_CONUS_ICS <- left_join(fs25_df, eco_sum_ICS, by = "FishID25k")


fish <- as(fishnet_25k, "Spatial")
fish$id <- row.names(fish)
fs25_df <- data.frame(fish)

GA_CONUS_ICS <- left_join(fs25_df, eco_sum_ICS, by = "FishID25k") %>%
  left_join(., wuw_eco_ICS, by = "FishID25k")