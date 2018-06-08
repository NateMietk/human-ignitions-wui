
# FPA BAE database from 2001-2015
# Overall totals
totals_bae <- as.data.frame(fpa_bae_wui) %>%
  group_by() %>%
  summarise(totfirearea = sum(wui_area_km2))

totals_class <- as.data.frame(fpa_bae_wui_wui) %>%
  group_by(class) %>%
  summarise(tot_class_area = first(total_class_area))

totals_class_coarse <- as.data.frame(fpa_bae_wui_wui) %>%
  group_by(class, class_coarse) %>%
  summarise(tot_class_area = first(total_class_area)) %>%
  group_by(class_coarse) %>%
  summarise(tot_class_coarse_area = sum(tot_class_area))

# Overall totals by CAUSE
bae_cause <- as.data.frame(fpa_bae_wui_wui) %>%
  group_by(ignition) %>%
  summarise(totfirearea = sum(wui_area_km2))

# Overall totals by CLASS
bae_class <- as.data.frame(fpa_bae_wui_wui) %>%
  group_by(class) %>%
  summarise(totfirearea = sum(wui_area_km2),
            total_class_area = first(total_class_area)) %>%
  mutate(pct_firewui_tot = (totfirearea/total_class_area)*100)

bae_class_coarse <- as.data.frame(fpa_bae_wui_wui) %>%
  group_by(class_coarse) %>%
  summarise(totfirearea = sum(wui_area_km2)) %>%
  left_join(., totals_class_coarse, by = 'class_coarse') %>%
  mutate(pct_firewui_tot = (totfirearea/tot_class_coarse_area)*100)

# Overall totals by CLASS AND CAUSE
bae_class <- fpa_bae_wui_wui %>%
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(totfirearea = sum(wui_area_km2))

totals_by_cause <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(firearea = sum(wui_area_km2),
            firesize = mean(wui_area_km2),
            firesize_sd = sd(wui_area_km2),
            firesize_90 = quantile(wui_area_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., bae_class, by = "class") %>%
  mutate(pct_firearea = (firearea/totals_bae$totfirearea)*100,
         pct_firewui = (firearea/class_area)*100)

# Summarize FPA BAE data by YEAR
totals_by_wuiburn_year <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  group_by(class, discovery_year) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firearea = (firearea/class_area)*100) %>%
  filter(class == "Wildlands")

mean(totals_by_wuiburn_year$pct_firearea)
mean(totals_by_wuiburn_year$firearea)

# Summarize FPA BAE data by CLASS
totals_bae_class_cause <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firearea = (firearea/class_area)*100) %>%
  filter(class == "VLD")

# Summarize FPA BAE data by CAUSE AND CLASS
totals_bae_class_cause <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firearea = (firearea/class_area)*100) %>%
  filter(class == "VLD")

# Summarize FPA BAE data by CAUSE AND CLASS AND SIZE
totals_bae <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(totfirearea = sum(wui_area_km2))

totals_bae_class_cause <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels = c("Small", "Large", "Very Large"))) %>%
  group_by(class, ignition, sizeclass) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., totals_bae, by = c("class", "ignition")) %>%
  mutate(pct_firearea = (firearea/totfirearea)*100,
         pct_firewui = (firearea/class_area)*100) %>%
  filter(class == "WUI")

# Summarize FPA BAE data by CAUSE AND CLASS AND SIZE AND REGION
totals_bae <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(totfirearea = sum(wui_area_km2))

totals_bae_class_cause <- fpa_bae_wui %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels = c("Small", "Large", "Very Large"))) %>%
  group_by(class, ignition, sizeclass, region) %>%
  summarise(n = n(),
            firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., totals_bae, by = c("class", "ignition")) %>%
  mutate(pct_firearea = (firearea/totfirearea)*100,
         pct_firewui = (firearea/class_area)*100) %>%
  filter(class == "WUI" & ignition == "Human")
