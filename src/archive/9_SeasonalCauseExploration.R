# What is the total number of fires in the east during the spring casued by arson and debris burning?
reg_cause_seasons <- wuw_short %>%
  mutate(region = ifelse(Region == "North East", "East",
                         ifelse(Region == "South East", "East",
                                ifelse(Region == "West", "West",
                                       "Central"))),
         Seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10)) %>%
  group_by(region, Seasons) %>%
  summarise(tot_fire = n()) %>%
  filter(region == "East" & Seasons == "Spring")


east_cause_spring <- wuw_short %>%
  mutate(region = ifelse(Region == "North East", "East",
                         ifelse(Region == "South East", "East",
                                ifelse(Region == "West", "West",
                                       "Central"))),
         Seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(STAT_CAUSE_DESCR == "Arson" | STAT_CAUSE_DESCR == "Debris Burning") %>%
  group_by(region, STAT_CAUSE_DESCR, Seasons) %>%
  summarise(seasonal_cause_n = n()) %>%
  filter(region == "East" & Seasons == "Spring") %>%
  select(region, STAT_CAUSE_DESCR, seasonal_cause_n) %>%
  left_join(., reg_cause_seasons, by = "region") %>%
  mutate(pct_tot = (seasonal_cause_n/tot_fire)*100)

# What is the total number of fires in the east regardless of season that are casued by arson and debris burning?
east_cause <- wuw_short %>%
  mutate(region = ifelse(Region == "North East", "East",
                         ifelse(Region == "South East", "East",
                                ifelse(Region == "West", "West",
                                       "Central"))),
         Seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(STAT_CAUSE_DESCR == "Arson" | STAT_CAUSE_DESCR == "Debris Burning") %>%
  group_by(region, STAT_CAUSE_DESCR) %>%
  summarise(stat_n = n()) %>%
  filter(region == "East")

totals_reg <- wuw_short %>%
  mutate(region = ifelse(Region == "North East", "East",
                         ifelse(Region == "South East", "East",
                                ifelse(Region == "West", "West",
                                       "Central"))),
         Class = classify_new_categories(WUICLASS10)) %>%
  group_by(region) %>%
  summarise(stat_n = n()) %>%
  filter(region == "East") %>%
  left_join(., east_cause, by = "region") %>%
  mutate(pct_tot = (stat_n.y/stat_n.x)*100)


wuw_short_cln_reg %>%
  filter(region == "East" & Seasons == "Spring") %>%
  ggplot() + 
  geom_line(aes(x = FIRE_YEAR, y = stat_n)) +
  facet_wrap(~STAT_CAUSE_DESCR)