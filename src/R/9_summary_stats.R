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

# Human related consequences
humcost_cause <- wuw_eco_ICS %>% 
  group_by(cause) %>%
  summarise(totcosts = sum(costs)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  mutate(pct_t = totcosts/t)
wuw_eco_ICS %>% 
     group_by(Class, cause) %>%
     summarise(costs = sum(costs)) %>%
     left_join(., humcost_cause, by = "Class") %>%
  as.data.frame(.) %>%
  mutate(pct = costs/totcosts)

humdestroy_cause <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(cause) %>%
  summarise(totdestroyed = sum(home.destroyed)) %>%
  as.data.frame(.) %>%
  select(-geom)

humdeath_cause <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(cause) %>%
  summarise(totdeath = sum(fatalities)) %>%
  as.data.frame(.) %>%
  select(-geom)

person_cause <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(cause) %>%
  summarise(totperson = sum(tot.pers)) %>%
  as.data.frame(.) %>%
  select(-geom)

wuw_eco_ICS %>% 
  group_by(Class, cause) %>%
  summarise(person = sum(tot.pers)) %>%
  left_join(., person_cause, by = "cause")

aerial_cause <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(cause) %>%
  summarise(totaerial = sum(tot.aerial)) %>%
  as.data.frame(.) %>%
  select(-geom)

wuw_eco_ICS %>% 
  group_by(Class, cause) %>%
  summarise(aerial = sum(tot.aerial)) %>%
  left_join(., aerial_cause, by = "cause")

agency_cause <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(cause) %>%
  summarise(totagency = sum(max.agency.support)) %>%
  as.data.frame(.) %>%
  select(-geom)

wuw_eco_ICS %>% 
  group_by(Class, cause) %>%
  summarise(agency = sum(max.agency.support)) %>%
  left_join(., agency_cause, by = "cause")

# Number of human related costs in the WUI

allfires <- wuw_eco_ICS %>% 
  group_by(syear) %>%
  summarise(tot_costs = sum(costs))

allfires <- as.data.frame(allfires) %>%
  select(-geom)

yrly_costs_wui_h <- wuw_eco_ICS %>%
  filter(Class == "WUI" & cause == "Human") %>%
  group_by(syear) %>%
  summarise(costs = sum(costs)) %>%
  left_join(., allfires, by = "syear") %>%
  mutate(yrly_costs = (costs/tot_costs)*100)

mean_costs_wui_h <- mean(yrly_costs_wui_h$yrly_costs)

# Number of human related homes destroyed in the WUI
allfires <- wuw_eco_ICS %>% 
  group_by(syear) %>%
  summarise(tot_destroy = sum(home.destroyed))

allfires <- as.data.frame(allfires) %>%
  select(-geom)

yrly_destroy_wui_h <- wuw_eco_ICS %>%
  filter(Class == "WUI" & cause == "Human") %>%
  group_by(syear) %>%
  summarise(destroy = sum(home.destroyed)) %>%
  left_join(., allfires, by = "syear") %>%
  mutate(yrly_destroy = (destroy/tot_destroy)*100)

sum_destroy_wui_h <- sum(yrly_destroy_wui_h$destroy)
mean_destroy_wui_h <- mean(yrly_destroy_wui_h$yrly_destroy)

# Number of human related fatalities in the WUI
allfires <- wuw_eco_ICS %>% 
  group_by(syear) %>%
  summarise(tot_fatalities = sum(fatalities))

allfires <- as.data.frame(allfires) %>%
  select(-geom)

yrly_fatalities_wui_h <- wuw_eco_ICS %>%
  filter(Class == "WUI" & cause == "Human") %>%
  group_by(syear) %>%
  summarise(deaths = sum(fatalities)) %>%
  left_join(., allfires, by = "syear") %>%
  mutate(yrly_fatalities = (deaths/tot_fatalities)*100)

sum_fatalities_wui_h <- sum(yrly_fatalities_wui_h$deaths)
mean_fatalities_wui_h <- mean(yrly_fatalities_wui_h$yrly_fatalities)

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
  group_by( cause) %>%
  summarise(costs = sum(costs),
            destory = sum(home.destroyed),
            threat = sum(home.threat),
            lives = sum(fatalities))

# number of people living in the WUI
t <- wuw_eco_wui %>%
  distinct(., BLK10, .keep_all = TRUE) %>%
  group_by(Class) %>%
  summarise(pop = sum(POP10),
            homes = sum(HHU10),
            area = sum(AREA_km2))

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



