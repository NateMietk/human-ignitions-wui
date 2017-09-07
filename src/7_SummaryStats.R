# Summary stats for overview table ----------------------------------------
wuw_eco_poly %>%
  group_by(IGNITION) %>%
  summarise(n_cnt = n(),
            mean = mean(FIRE_SIZE),
            median = median(FIRE_SIZE))

Full_Maxseasons %>%
  group_by(IGNITION, Class) %>%
  summarise(nfall = sum(Fall),
            nwint = sum(Winter),
            nspr = sum(Spring),
            nsumm = sum(Summer))

Eco_IQR %>%
  group_by(Class, IGNITION) %>%
  summarise(mean_iqr = mean(iqr))

# % WUI Burned by human and lightning ignitions per year
wuw_eco_bae %>%
  group_by(FIRE_YEAR, Class) %>%
  summarise(tot_fire = sum(AREA_km2)) %>%
  filter(Class == "WUI") %>%
  mutate(yrly_burn = (tot_fire/784320)*100)

eco_sum_bae2 <- mean(eco_sum_bae2$yrly_burn)

# Number of human related ignitions in the WUI
allfires <- wuw_eco_poly %>% 
  summarise(tot_fire = n())

humanWUIfires <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(tot_fire = n()) %>%
  filter(Class == "WUI" & IGNITION == "Human")

PctWUIHuamn <- (humanWUIfires$tot_fire/allfires$tot_fire)*100

# Social Impact of human started fires in the WUI

wuw_eco_ICS %>%
  group_by(Class, Ignition) %>%
  summarise(costs = sum(costs),
            destory = sum(destroy),
            threat = sum(threat),
            lives = sum(deaths))

# number of people living in the WUI
t <- wuw_eco_wui %>%
  distinct(., BLK10, .keep_all = TRUE) %>%
  group_by(Class) %>%
  summarise(pop = sum(POP10),
            homes = sum(HHU10))

# For the level 1 ecoregions --------------------------------------------
wui_stat <- wuw_eco_wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(NA_L1NAME, Class) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup()  %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  spread(Class, ClassArea)

ff <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by( Class, IGNITION) %>% 
  summarise(tot_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, tot_fire)

bae_stats <- wuw_eco_bae %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(burn_area = sum(AREA_km2)) %>%
  ungroup() %>%
  spread(IGNITION, burn_area)

Eco_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(NA_L1NAME, Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason)

conus_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason)


cost_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, Ignition) %>% 
  summarise(totcosts = sum(costs)) %>%
  ungroup() %>%
  spread(Ignition, totcosts)

hdes_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(NA_L1NAME, Class, Ignition) %>% 
  summarise(totdes = sum(destroy)) %>%
  ungroup() %>%
  spread(Ignition, totdes)

fat_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(NA_L1NAME, Class, Ignition) %>% 
  summarise(totdea = sum(deaths)) %>%
  ungroup() %>%
  spread(Ignition, totdea)

# For the state
wui_stat <- wuw_eco_wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(STUSPS, Class) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup()  %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  spread(Class, ClassArea)

ff <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, IGNITION) %>% 
  summarise(tot_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, tot_fire) %>%
  mutate(h_ff = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_ff = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_ff = h_ff/(h_ff+l_ff)*100) %>%
  select(STUSPS, Class, h_ff, l_ff, pct_ff)

bae_stats <- wuw_eco_bae %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, IGNITION) %>% 
  summarise(burn_area = sum(AREA_km2)) %>%
  ungroup() %>%
  spread(IGNITION, burn_area) %>%
  mutate(h_bae = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_bae = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_bae = h_bae/(h_bae+l_bae)*100) %>%
  select(STUSPS, Class, h_bae, l_bae, pct_bae)

Eco_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason) %>%
  mutate(h_fsea = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_fsea = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_fsea = ifelse(h_fsea > l_fsea, h_fsea/(l_fsea)*100, "--")) %>%
  select(STUSPS, Class, h_fsea, l_fsea, pct_fsea)

conus_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason)


cost_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, Ignition) %>% 
  summarise(totcosts = sum(costs)) %>%
  ungroup() %>%
  spread(Ignition, totcosts) %>%
  mutate(h_cost = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_cost = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_cost = h_cost/(h_cost+l_cost)*100) %>%
  select(STUSPS, Class, h_cost, l_cost, pct_cost)

hdes_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, Ignition) %>% 
  summarise(totdes = sum(destroy)) %>%
  ungroup() %>%
  spread(Ignition, totdes) %>%
  mutate(h_des = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_des = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_des = h_des/(h_des+l_des)*100) %>%
  select(STUSPS, Class, h_des, l_des, pct_des)

fat_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, Ignition) %>% 
  summarise(totdea = sum(deaths)) %>%
  ungroup() %>%
  spread(Ignition, totdea) %>%
  mutate(h_fat = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_fat = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_fat = h_fat/(h_fat+l_fat)*100) %>%
  select(STUSPS, Class, h_fat, l_fat, pct_fat)

State_Stats <- ff %>%
  left_join(., bae_stats, by = c("STUSPS", "Class")) %>%
  left_join(., Eco_IQRstats, by = c("STUSPS", "Class")) %>%
  left_join(., cost_stats, by = c("STUSPS", "Class")) %>%
  left_join(., hdes_stats, by = c("STUSPS", "Class")) %>%
  left_join(., fat_stats, by = c("STUSPS", "Class")) %>%
  mutate(h_ff = round(ifelse(is.na(as.numeric(h_ff)), 0 , as.numeric(h_ff)),2),
         l_ff = round(ifelse(is.na(as.numeric(l_ff)), 0 , as.numeric(l_ff)),2),
         pct_ff = round(ifelse(is.na(as.numeric(pct_ff)), 0 , as.numeric(pct_ff)),2),
         h_bae = round(ifelse(is.na(as.numeric(h_bae)), 0 , as.numeric(h_bae)),2),
         l_bae = round(ifelse(is.na(as.numeric(l_bae)), 0 , as.numeric(l_bae)),2),
         pct_bae = round(ifelse(is.na(as.numeric(pct_bae)), 0 , as.numeric(pct_bae)),2),
         h_fsea = round(ifelse(is.na(as.numeric(h_fsea)), 0 , as.numeric(h_fsea)),2),
         l_fsea = round(ifelse(is.na(as.numeric(l_fsea)), 0 , as.numeric(l_fsea)),2),
         pct_fsea = round(ifelse(is.na(as.numeric(pct_fsea)), 0 , as.numeric(pct_fsea)),2),
         h_cost = round(ifelse(is.na(as.numeric(h_cost)), 0 , as.numeric(h_cost)),2),
         l_cost = round(ifelse(is.na(as.numeric(l_cost)), 0 , as.numeric(l_cost)),2),
         pct_cost = round(ifelse(is.na(as.numeric(pct_cost)), 0 , as.numeric(pct_cost)),2), 
         h_des = round(ifelse(is.na(as.numeric(h_des)), 0 , as.numeric(h_des)),2),
         l_des = round(ifelse(is.na(as.numeric(l_des)), 0 , as.numeric(l_des)),2),
         pct_des = round(ifelse(is.na(as.numeric(pct_des)), 0 , as.numeric(pct_des)),2),
         h_fat = round(ifelse(is.na(as.numeric(h_fat)), 0 , as.numeric(h_fat)),2),
         l_fat = round(ifelse(is.na(as.numeric(l_fat)), 0 , as.numeric(l_fat)),2),
         pct_fat = round(ifelse(is.na(as.numeric(pct_fat)), 0 , as.numeric(pct_fat)),2))

State_Stats_WUI_Wild <- State_Stats %>%
  filter(Class != "VLD")

State_Stats_VLD <- State_Stats %>%
  filter(Class == "VLD")              

State_Stats_CONUS <- State_Stats %>%
  select(., -STUSPS) %>%
  group_by(Class) %>%
  summarise_each(funs(sum)) %>%
  ungroup()



