# WUI/Ecoregions/State areas ----------------------------------------------

area_grp_st <- wui_state_eco %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, STUSPS, State_Area_km2) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup()  %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea)

area_grp_eco <- wui_state_eco %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, US_L3NAME, EcReg_Area_km2) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea)

area_grp_fish50 <- wui_state_eco %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, FishID_50k) %>%
  summarise(ClassArea_Fish = sum(AREA_km2)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea_Fish) %>%
  mutate(VLD = ifelse(is.na(VLD), 0, VLD),
         Wildlands = ifelse(is.na(Wildlands), 0, Wildlands),
         WUI = ifelse(is.na(WUI), 0, WUI))

area_grp_fish25 <- wui_state_eco %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, FishID_25k) %>%
  summarise(ClassArea_Fish = sum(AREA_km2)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea_Fish) %>%
  mutate(VLD = ifelse(is.na(VLD), 0, VLD),
         Wildlands = ifelse(is.na(Wildlands), 0, Wildlands),
         WUI = ifelse(is.na(WUI), 0, WUI))

# Prep key aggregate datasets -------------------------------------------
wuw_eco_poly <- fpa_wui %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         FireSize = classify_fire_size_cl(fire_size_km2),
         Class = classify_new_categories(WUICLASS10),
         Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

wuw_eco_bae <- fpa_bae %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10),
         Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_fish25, by = "FishID_25k")

wuw_eco_wui <- wui_state_eco %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

