
wui_209 <- st_read(file.path(ics_spatial, "ics209_wui_conus.gpkg")) %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  filter(class == "WUI" | class == "VLD" | class == "Wildlands") %>%
  mutate(region = as.factor(
    if_else(
      ecoreg1.name %in% c("EASTERN TEMPERATE FORESTS","TROPICAL WET FORESTS","NORTHERN FORESTS"), 
      "East",
      if_else(
        ecoreg1.name %in% c("NORTH AMERICAN DESERTS", "SOUTHERN SEMIARID HIGHLANDS","TEMPERATE SIERRAS",
                            "MEDITERRANEAN CALIFORNIA","NORTHWESTERN FORESTED MOUNTAINS",
                            "MARINE WEST COAST FOREST"), 
        "West", "Central"))))
    
wui_209 <- wui_209 %>%
  mutate(
    area_km2 = if_else(incident_unique_id == "CA-SQF-2511|2006|1", 4.9, 
                       if_else(incident_unique_id == "CO-PSF-283|2002|1", 16.5, area_km2)),
  class = clean_class(incident_unique_id,  as.character(class)),
    cause = if_else(
      incident_unique_id %in% c("ID-BOD-000553|2011|1","CA-RRU-062519|2005|1","NC-NCS-08081071|2008|1",
                                "CA-SCU-3094|2008|1","OR-SIF-003|2002|1"),
      "Human", as.character(cause)))

names(wui_209) %<>% tolower

wuw_area <- data.table(class=c("WUI", "VLD", "Wildlands"), 
                       class_area = c(784320, 2260783, 2598246))

wui_impact <- wui_209 %>%
  mutate(wui_impact = if_else(home.destroyed == 0 & comm.destroyed == 0 & home.threat == 0 & comm.threat == 0, "no_impact", "impact"))

# Overall totals of impact/no impact
wui_totals_risk <- wui_impact %>% 
  filter(cause != "Unk") %>%
  group_by(wui_impact) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            death = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs)) %>%
  as.data.frame(.) %>%
  select(-geom)

# Overall at risk WUI by CAUSE
risk_by_cause <- wui_impact %>% 
  filter(cause != "Unk") %>%
  group_by(cause, wui_impact) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom)%>%
  mutate(pct_destroyed = (destroyed/wui_totals_risk$destroyed)*100,
         pct_threatened = (threatened/wui_totals_risk$threatened)*100,
         pct_deaths = (deaths/wui_totals_risk$death)*100,
         pct_person = (person/wui_totals_risk$person)*100,
         pct_aerial = (aerial/wui_totals_risk$aerial)*100,
         pct_costs = (costs/wui_totals_risk$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

risk_by_cause_slim <- risk_by_cause %>%
  mutate(totfirefreq = fire_num,
         totfirearea = destroyed,
         totseasonlength = threatened,
         totdeaths = deaths, 
         totperson = person,
         totaerial = aerial,
         totcosts = costs) %>%
  dplyr::select(cause, totfirefreq, totfirearea, totseasonlength,
                totdeaths, totperson, totaerial, totcosts)

# Overall at risk WUI by CLASS
risk_by_class <- wui_impact %>% 
  filter(cause != "Unk") %>%
  group_by(class, wui_impact) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom)%>%
  mutate(pct_destroyed = (destroyed/wui_totals_risk$destroyed)*100,
         pct_threatened = (threatened/wui_totals_risk$threatened)*100,
         pct_deaths = (deaths/wui_totals_risk$death)*100,
         pct_person = (person/wui_totals_risk$person)*100,
         pct_aerial = (aerial/wui_totals_risk$aerial)*100,
         pct_costs = (costs/wui_totals_risk$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

risk_by_class_slim <- risk_by_class %>%
  mutate(totfirefreq = fire_num,
         totfirearea = destroyed,
         totseasonlength = threatened,
         totdeaths = deaths, 
         totperson = person,
         totaerial = aerial,
         totcosts = costs) %>%
  dplyr::select(class, totfirefreq, totfirearea, totseasonlength,
                totdeaths, totperson, totaerial, totcosts)

# Overall at risk WUI by CLASS AND CAUSE
risk_by_class_cause <- wui_impact %>% 
  filter(cause != "Unk") %>%
  group_by(class, cause, wui_impact,ecoreg1.name) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom)%>%
  mutate(pct_destroyed = (destroyed/wui_totals_risk$destroyed)*100,
         pct_threatened = (threatened/wui_totals_risk$threatened)*100,
         pct_deaths = (deaths/wui_totals_risk$death)*100,
         pct_person = (person/wui_totals_risk$person)*100,
         pct_aerial = (aerial/wui_totals_risk$aerial)*100,
         pct_costs = (costs/wui_totals_risk$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>%
  arrange(cause) %>%
  filter(class == "Wildlands") 

risk_by_class_cause_slim <- risk_by_class_cause %>%
  mutate(totfirefreq = fire_num,
         totfirearea = destroyed,
         totseasonlength = threatened,
         totdeaths = deaths, 
         totperson = person,
         totaerial = aerial,
         totcosts = costs) %>%
  dplyr::select(class, cause, totfirefreq, totfirearea, totseasonlength,
                totdeaths, totperson, totaerial, totcosts)

# Overall at risk WUI by CLASS AND CAUSE AND YEAR
risk_by_class_cause <- wui_impact %>% 
  filter(cause != "Unk") %>%
  group_by(syear, cause, class, wui_impact) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  mutate(pct_destroyed = (destroyed/wui_totals_risk$destroyed)*100,
         pct_threatened = (threatened/wui_totals_risk$threatened)*100,
         pct_deaths = (deaths/wui_totals_risk$death)*100,
         pct_person = (person/wui_totals_risk$person)*100,
         pct_aerial = (aerial/wui_totals_risk$aerial)*100,
         pct_costs = (costs/wui_totals_risk$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

risk_by_class_cause_slim <- risk_by_class_cause %>%
  mutate(totfirefreq = fire_num,
         totfirearea = destroyed,
         totseasonlength = threatened,
         totdeaths = deaths, 
         totperson = person,
         totaerial = aerial,
         totcosts = costs) %>%
  dplyr::select(class, cause, totfirefreq, totfirearea, totseasonlength,
                totdeaths, totperson, totaerial, totcosts)


# Overall at risk WUI by CLASS AND CAUSE AND REGION
risk_by_class_cause <- wui_impact %>% 
  filter(cause != "Unk") %>%
  mutate(sizeclass = classify_fire_size_cl(area_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, cause, class, wui_impact) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  mutate(pct_destroyed = (destroyed/wui_totals_risk$destroyed)*100,
         pct_threatened = (threatened/wui_totals_risk$threatened)*100,
         pct_deaths = (deaths/wui_totals_risk$death)*100,
         pct_person = (person/wui_totals_risk$person)*100,
         pct_aerial = (aerial/wui_totals_risk$aerial)*100,
         pct_costs = (costs/wui_totals_risk$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

# Overall totals
wui_totals <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by() %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            death = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs)) %>%
  as.data.frame(.) %>%
  select(-geom)

# Overall totals by CAUSE
totals_by_cause <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom)%>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

totals_by_cause_slim <- totals_by_cause %>%
  mutate(totfirefreq = fire_num,
         totfirearea = destroyed,
         totseasonlength = threatened,
         totdeaths = deaths, 
         totperson = person,
         totaerial = aerial,
         totcosts = costs) %>%
  dplyr::select(cause, totfirefreq, totfirearea, totseasonlength,
                totdeaths, totperson, totaerial, totcosts)

# Overall totals by CLASS
totals_by_class <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(class) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom)%>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

totals_by_class_slim <- totals_by_class %>%
  mutate(totfirefreq = fire_num,
         totfirearea = destroyed,
         totseasonlength = threatened,
         totdeaths = deaths, 
         totperson = person,
         totaerial = aerial,
         totcosts = costs) %>%
  dplyr::select(class, totfirefreq, totfirearea, totseasonlength,
                totdeaths, totperson, totaerial, totcosts)

# Overall totals by CLASS AND CAUSE
totals_by_class_cause <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(class, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom)%>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>%
  arrange(cause) %>%
  filter(class == "Wildlands") 

totals_by_class_cause_slim <- totals_by_class_cause %>%
  mutate(totfirefreq = fire_num,
         totfirearea = destroyed,
         totseasonlength = threatened,
         totdeaths = deaths, 
         totperson = person,
         totaerial = aerial,
         totcosts = costs) %>%
  dplyr::select(class, cause, totfirefreq, totfirearea, totseasonlength,
                totdeaths, totperson, totaerial, totcosts)

# Overall totals by SIZE
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  mutate(sizeclass = classify_fire_size_cl(area_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

# Overall totals by SIZE AND CAUSE
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  mutate(sizeclass = classify_fire_size_cl(area_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>% 
  arrange(cause)

# Overall totals by SIZE AND CLASS
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  mutate(sizeclass = classify_fire_size_cl(area_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, class) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>% 
  arrange(class)
  
# Overall totals by SIZE AND CLASS AND CAUSE
totals_by_cause_class_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  mutate(sizeclass = classify_fire_size_cl(area_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, class, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>% 
  filter(class == "Wildlands")
  arrange(class, cause)

# Overall totals by REGION
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(region) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

# Overall totals by REGION AND CAUSE
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(region, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>% 
  arrange(cause)

# Overall totals by REGION AND CLASS AND CAUSE
totals_by_cause_class_seasons <- wui_209 %>% 
  filter(cause != "Unk") %>%
  mutate(sizeclass = classify_fire_size_cl(area_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(region, sizeclass, class, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>% 
  filter(class == "WUI") %>%
  arrange(cause, sizeclass, region)

# Overall totals by SEASON
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(seasons) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) 

# Overall totals by SEASON AND CAUSE
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(seasons, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>% 
  arrange(cause)

# Overall totals by SEASON AND CLASS
totals_by_cause_sizes <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(region, class, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  # mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
  #        pct_threatened = (threatened/wui_totals$threatened)*100,
  #        pct_deaths = (deaths/wui_totals$death)*100,
  #        pct_person = (person/wui_totals$person)*100,
  #        pct_aerial = (aerial/wui_totals$aerial)*100,
  #        pct_costs = (costs/wui_totals$costs)*100,
  #        cost_per_km2 = (costs/totarea),
  #        cost_per_firefreq = costs/fire_num) %>% 
  arrange(class)

# Overall totals by SEASON AND CLASS AND CAUSE
totals_by_cause_class_seasons <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(seasons, class, cause) %>%
  summarise(fire_num = n(),
            destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  mutate(pct_destroyed = (destroyed/wui_totals$destroyed)*100,
         pct_threatened = (threatened/wui_totals$threatened)*100,
         pct_deaths = (deaths/wui_totals$death)*100,
         pct_person = (person/wui_totals$person)*100,
         pct_aerial = (aerial/wui_totals$aerial)*100,
         pct_costs = (costs/wui_totals$costs)*100,
         cost_per_km2 = (costs/totarea),
         cost_per_firefreq = costs/fire_num) %>% 
  filter(class == "WUI")

# Overall totals by YEAR
totals_by_year <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(syear) %>%
  summarise(totdestroyed = sum(home.destroyed, comm.destroyed),
            totdeath = sum(fatalities),
            totperson = sum(tot.pers),
            totaerial = sum(tot.aerial),
            totcosts = sum(costs)) %>%
  as.data.frame(.) %>%
  select(-geom)

# Overall totals by CAUSE AND YEAR
totals_by_year_cause_class <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(syear, cause) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  left_join(., totals_by_year, by = "syear") %>%
  mutate(pct_destroyed = (destroyed/totdestroyed)*100,
         pct_deaths = (deaths/totdeath)*100,
         pct_person = (person/totperson)*100,
         pct_aerial = (aerial/totaerial)*100,
         pct_costs = (costs/totcosts)*100,
         cost_per_km2 = (costs/totarea)) %>%
  arrange(desc(cost_per_km2,cause)) 

# Overall totals by CLASS AND YEAR
totals_by_year_cause_class <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(syear, class) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  left_join(., totals_by_year, by = "syear") %>%
  mutate(pct_destroyed = (destroyed/totdestroyed)*100,
         pct_deaths = (deaths/totdeath)*100,
         pct_person = (person/totperson)*100,
         pct_aerial = (aerial/totaerial)*100,
         pct_costs = (costs/totcosts)*100,
         cost_per_km2 = (costs/totarea)) %>%
  arrange(desc(cost_per_km2, class)) 
 
totals_by_year_mean <- totals_by_year_cause_class %>%
  select(-syear, -class) %>%
  group_by(cause) %>%
  summarise_all(mean) %>%
  ungroup()

# How different are the east and west in terms of ignitons class?
totals_209 <- wui_209 %>% 
  as.data.frame(.) %>%
  group_by( cause, class) %>%
  summarise(totdestroyed = sum(home.destroyed, comm.destroyed),
            totdeath = sum(fatalities),
            totperson = sum(tot.pers),
            totaerial = sum(tot.aerial),
            totcosts = sum(costs)) 

totals_by_sizeclass <- wui_209 %>% 
  as.data.frame(.) %>%
  filter(cause != "Unk") %>%
  group_by(region, cause, class) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  left_join(., totals_209, by = c("cause", "class")) %>%
  mutate(pct_destroyed = (destroyed/totdestroyed)*100,
         pct_deaths = (deaths/totdeath)*100,
         pct_person = (person/totperson)*100,
         pct_aerial = (aerial/totaerial)*100,
         pct_costs = (costs/totcosts)*100,
         cost_per_km2 = (costs/totarea)) %>%
  arrange(desc(cause, syear)) %>%
  filter(class == "VLD" & cause == "Human")

totals_by_year_cause_class <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(syear, cause, class) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., totals_by_year, by = "syear") %>%
  mutate(pct_destroyed = (destroyed/totdestroyed)*100,
         pct_deaths = (deaths/totdeath)*100,
         pct_person = (person/totperson)*100,
         pct_aerial = (aerial/totaerial)*100,
         pct_costs = (costs/totcosts)*100,
         cost_per_km2 = (costs/totarea),
         cost_norm_class_area = (costs/class_area)/totarea) %>%
  arrange(desc(cause, syear)) %>%
  filter(class == "Wildlands" & cause == "Lightning")

totals_by_year_cause_class %>%
  transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot() +
  geom_bar(aes(x = cause, y = cost_norm_class_area,
               color = cause, fill = cause), 
           position = "dodge", stat = "identity") +
  theme_pub() +
  theme(legend.position = c(0.8, 0.8)) +
  facet_wrap( ~ class)
  