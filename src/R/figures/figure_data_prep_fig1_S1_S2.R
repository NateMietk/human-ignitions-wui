bu_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))

ics_totals <- as.data.frame(wui_209) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(fishid50k, class_coarse) %>%
  summarise(tot_costs = sum(costs)) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_suppresscosts(tot_costs),
         ptsz_n = ifelse(is.na(ptsz_n) | is.nan(ptsz_n), 0, ptsz_n)) %>%
  filter(ptsz_n != 0)

bu_totals <- as.data.frame(bu_complete_cleaned) %>%
  mutate_if(is.character, as.factor) %>%
  filter(built_class == 'Residential' & discovery_year != 0) %>%
  group_by(fishid50k, class_coarse) %>%
  summarise(tot_bu = sum(build_up_count_no_zero_0)) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_bu(tot_bu),
         ptsz_n = ifelse(is.na(ptsz_n) | is.nan(ptsz_n), 0, ptsz_n)) 

bae_totals <- as.data.frame(fpa_bae_wui) %>%
  setNames(tolower(names(.))) %>%
  group_by(fishid50k, class_coarse) %>%
  summarise(fire_freq = n(),
            tot_burn_area = sum(fire_size_km2)) %>%
  mutate(frsz_cl = classify_raw_fire_size(tot_burn_area)) 

class_totals <- as.data.frame(fpa_wui) %>%
  group_by(fishid50k, class_coarse) %>%
  summarise(tot_fire = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire),
         ptsz_n = ifelse(is.na(ptsz_n) | is.nan(ptsz_n), 0, ptsz_n)) 

wui_fish50k_sum <- ungroup(read_rds(file.path(wui_out, "wui_fish50k_sum.rds"))) %>%
  filter(year == 2010) %>%
  mutate(class_coarse =  as.factor(ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                                           ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class))))) %>%
  dplyr::select(-year, -class) %>%
  group_by(fishid50k, class_coarse) %>%
  summarise(total_fishid50k_area = sum(total_fishid50k_area))

fire_density <- as.data.frame(fpa_wui) %>%
  group_by(fishid50k, ignition, class_coarse) %>%
  summarise(n_fire = n()) %>%
  ungroup() %>%
  spread(ignition, n_fire) %>%
  left_join(., class_totals, by = c("class_coarse", "fishid50k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_fire-n_Human),
         n_light_den = (tot_fire-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100,
         n_den = ifelse(is.na(n_den) | is.nan(n_den), 0, n_den))

bu_density <- as.data.frame(bu_complete_cleaned) %>%
  filter(built_class == 'Residential' & discovery_year != 0) %>%
  group_by(fishid50k, ignition, class_coarse) %>%
  summarise(built_count = sum(build_up_count_no_zero_0)) %>%
  ungroup() %>%
  spread(ignition, built_count) %>%
  left_join(., bu_totals, by = c("class_coarse", "fishid50k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_bu-n_Human),
         n_light_den = (tot_bu-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100,
         n_den = ifelse(is.na(n_den) | is.nan(n_den), 0, n_den))

ics_density <- as.data.frame(wui_209) %>%
  group_by(fishid50k, cause, class_coarse) %>%
  summarise(costs = sum(costs)) %>%
  ungroup() %>%
  spread(cause, costs) %>%
  left_join(., ics_totals, by = c("class_coarse", "fishid50k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_costs-n_Human),
         n_light_den = (tot_costs-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100,
         n_den = ifelse(is.na(n_den) | is.nan(n_den), 0, n_den))

burn_area_density <- as.data.frame(fpa_bae_wui) %>%
  setNames(tolower(names(.))) %>%
  left_join(., wui_fish50k_sum, by = c("fishid50k", "class_coarse")) %>%
  group_by(fishid50k, ignition, class_coarse) %>%
  summarise(wui_area_km2 = sum(wui_area_km2),
            total_fishid50k_area = max(total_fishid50k_area)) %>%
  spread(ignition, wui_area_km2) %>%
  left_join(., bae_totals, by = c("class_coarse", "fishid50k")) %>%
  mutate(s_Human = ifelse(is.na(Human), 0, Human),
         s_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         tot_fire = (s_Human + s_Lightning),
         s_den = ifelse(is.na((1-(s_Lightning/tot_fire))*100), 0, (1-(s_Lightning/tot_fire))*100),
         pct_class_human = s_Human/total_fishid50k_area*100,
         pct_class_lightning = s_Lightning/total_fishid50k_area*100,
         pct_class_human = classify_pctbae(pct_class_human),
         pct_class_lightning = classify_pctbae(pct_class_lightning)) %>%
  na.omit(frsz_cl)

conus_ff <- left_join(fs50_df, fire_density, by = "fishid50k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 

conus_burn_area <- left_join(fs50_df, burn_area_density, by = "fishid50k")  %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 

conus_bu <- left_join(fs50_df, bu_density, by = "fishid50k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 

conus_ics <- left_join(fs50_df, ics_density, by = "fishid50k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 