
# How many fires were started by HUMANS in the WUI
totals <- as.data.frame(fpa_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by() %>%
  summarise(all_firefreq = n(),
            all_firearea = sum(fire_size_km2),
            all_seasonlength = IQR(discovery_doy))

totals_by_class <- as.data.frame(fpa_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(class_coarse) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy))

totals_by_cause_class <- as.data.frame(fpa_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(class_coarse, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., totals_by_class, by = 'class_coarse') %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100,
         pct_of_all_firefreq = (firefreq/totals$all_firefreq)*100,
         pct_of_all_firearea = (firearea/totals$all_firearea)*100)

# What is the average yearly percent burn per CLASS
pct_burn_class_ci <- as.data.frame(fpa_bae_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  left_join(., coarse_wuw_area, by = c('decadal', 'class_coarse')) %>%
  group_by(discovery_year, class_coarse) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE),
            total_coarse_class_area = first(total_coarse_class_area)) %>%
  mutate(pct_yrly_burn = (sum_burned_area/total_coarse_class_area)*100) %>%
  group_by(class_coarse) %>%
  do(data.frame(rbind(smean.cl.boot(.$pct_yrly_burn, B = 10000)))) 

pct_burn_class_ci %>%
  transform(class_coarse = factor(class_coarse, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class_coarse, y = Mean)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) 

totals_class_ci <- as.data.frame(fpa_bae_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class_coarse) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE)) %>%
  group_by(class_coarse) %>%
  do(data.frame(rbind(smean.cl.boot(.$sum_burned_area, B = 10000)))) 

totals_class_ci %>%
  transform(class_coarse = factor(class_coarse, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class_coarse, y = Mean)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) 

# What is the average yearly percent burn per CLASS and IGNITION
pct_burn_class_cause_ci <- as.data.frame(fpa_bae_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE),
            total_class_area = first(total_class_area)) %>%
  mutate(pct_yrly_burn = (sum_burned_area/total_class_area)*100) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$pct_yrly_burn, B = 10000)))) 

p1 <- pct_burn_class_cause_ci %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average yearly percent burned (%)') +
  theme_pub() +
  theme(legend.position = 'none')

totals_burn_class_ci <- as.data.frame(fpa_bae_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE)) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$sum_burned_area, B = 10000)))) 

p2 <- totals_burn_class_ci %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average yearly burned (km2)') +
  theme_pub() +
  theme(legend.position = 'none')

# What is the average yearly percent homes threatened by HUMANS fires started in the WUI
pct_bu_totals <- as.data.frame(bu_cleaned) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by() %>%
  summarise(total_bu = sum(bu, na.rm = TRUE))

pct_bu_class_cause_ci <- as.data.frame(bu_cleaned) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_bu = sum(bu, na.rm = TRUE)) %>%
  mutate(pct_bu = (sum_bu/pct_bu_totals$total_bu)*100) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$pct_bu, B = 10000)))) 

p3 <- pct_bu_class_cause_ci %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average yearly threathened housing units via ZTrax (%)') +
  scale_y_continuous(limits = c(0,3)) +
  theme_pub() +
  theme(legend.position = 'none')

pct_house_totals <- as.data.frame(fpa_bae_wui) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by() %>%
  summarise(total_house = sum(house_units, na.rm = TRUE))

per_fire_house_tots <- as.data.frame(fpa_bae_wui) %>%
  group_by(fpa_id) %>%
  summarise(per_fire_house = sum(house_units, na.rm = TRUE)) %>%
  left_join(., fpa_wui, by = 'fpa_id') %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_house = sum(per_fire_house, na.rm = TRUE)) %>%
  mutate(pct_house = (sum_house/pct_house_totals$total_house)*100) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$pct_house, B = 10000)))) %>%
  na.omit()

p4 <- per_fire_house_tots %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average yearly threathened housing units via SILVIS lab (%)') +
  scale_y_continuous(limits = c(0,3)) +
  theme_pub() +
  theme(legend.position = 'none')

grid.arrange(p3, p4, nrow = 1)


# How many homes were threatened by HUMANS fires started in the WUI
tot_bu_class_cause_ci <- as.data.frame(bu_cleaned) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_bu = sum(bu, na.rm = TRUE)) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$sum_bu, B = 10000)))) 

p5 <- tot_bu_class_cause_ci %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean*0.00001, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower*0.00001, ymax = Upper*0.00001), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average yearly threathened housing units via ZTrax (in 10,000 units)') +
  scale_y_continuous(limits = c(0,17)) +
  theme_pub() +
  theme(legend.position = 'none')

per_fire_house_tots <- as.data.frame(fpa_bae_wui) %>%
  group_by(fpa_id) %>%
  summarise(per_fire_house = sum(house_units, na.rm = TRUE)) %>%
  left_join(., fpa_wui, by = 'fpa_id') %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_house = sum(per_fire_house, na.rm = TRUE)) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$sum_house, B = 10000)))) %>%
  filter(!is.na(class))

p6 <- per_fire_house_tots %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean*0.00001, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower*0.00001, ymax = Upper*0.00001), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Total threathened housing units via SILVIS lab (in 10,000 units)') +
  scale_y_continuous(limits = c(0,17)) +
  theme_pub() +
  theme(legend.position = 'none')

grid.arrange(p5, p6, nrow = 1)

# What are the housing unit relationships for MTBS fires only?
pct_bu_totals_mtbs <- as.data.frame(bu_cleaned) %>%
  filter(!is.na(mtbs_id)) %>% 
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by() %>%
  summarise(total_bu = sum(bu, na.rm = TRUE))

pct_bu_class_cause_ci_mtbs <- as.data.frame(bu_cleaned) %>%
  filter(!is.na(mtbs_id)) %>% 
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_bu = sum(bu, na.rm = TRUE)) %>%
  mutate(pct_bu = (sum_bu/pct_bu_totals_mtbs$total_bu)*100) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$pct_bu, B = 10000)))) 

p7 <- pct_bu_class_cause_ci_mtbs %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average yearly threathened housing units via ZTrax and MTBS (%)') +
  scale_y_continuous(limits = c(0,4)) +
  theme_pub() +
  theme(legend.position = 'none')

pct_house_totals_sil_mtbs <- as.data.frame(fpa_bae_wui) %>%
  filter(!is.na(mtbs_id)) %>% 
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by() %>%
  summarise(total_house = sum(house_units, na.rm = TRUE))

per_fire_house_tots_sil_mtbs <- as.data.frame(fpa_bae_wui) %>%
  filter(!is.na(mtbs_id)) %>% 
  group_by(fpa_id) %>%
  summarise(per_fire_house = sum(house_units, na.rm = TRUE)) %>%
  left_join(., fpa_wui, by = 'fpa_id') %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(sum_house = sum(per_fire_house, na.rm = TRUE)) %>%
  mutate(pct_house = (sum_house/pct_house_totals_sil_mtbs$total_house)*100) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$pct_house, B = 10000)))) %>%
  filter(!is.na(class))

p8 <- per_fire_house_tots_sil_mtbs %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  scale_y_continuous(limits = c(0,4)) +
  xlab('Class') + ylab('Average yearly threathened housing units via SILVIS and MTBS (%)') +
  theme_pub() +
  theme(legend.position = 'none')

grid.arrange(p7, p8, nrow = 1)


# How many homes were threatened by HUMANS fires started in the WUI
per_fire_bu_total <- as.data.frame(bu_cleaned) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$bu, B = 10000)))) 

p5 <- per_fire_bu_total %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average threathened housing units per fire via ZTrax') +
  scale_y_continuous(limits = c(0,170)) +
  theme_pub() +
  theme(legend.position = 'none')

per_fire_house_total <- as.data.frame(fpa_bae_wui) %>%
  group_by(fpa_id) %>%
  summarise(per_fire_house = sum(house_units, na.rm = TRUE)) %>%
  left_join(., fpa_wui, by = 'fpa_id') %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$per_fire_house, B = 10000)))) %>%
  filter(!is.na(class))

p6 <- per_fire_house_total %>%
  transform(class = factor(class, levels=c('Intermix WUI', "Interface WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = class, y = Mean, fill = ignition)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2,
                position=position_dodge(.7)) +
  xlab('Class') + ylab('Average threathened housing units per fire via SILVIS lab') +
  scale_y_continuous(limits = c(0,170)) +
  theme_pub() +
  theme(legend.position = 'none')

grid.arrange(p5, p6, nrow = 1)



## How different are the SILVIS housing estimates from the ZTrax

decade_silvis_vs_ztrax <- bu_wui_cleaned %>%
  group_by(year) %>%
  summarise(ztrax = sum(residential_built_up, na.rm = TRUE),
            silvis = sum(house_units, na.rm = TRUE)) %>%
  mutate(year = as.factor(year))  %>%
  gather(housing, value = c('ztrax', 'silvis'), -year) %>%
  mutate(value = `c("ztrax", "silvis")`) %>% dplyr::select(-`c("ztrax", "silvis")`) %>%
  ggplot(aes(x = year, y = value*0.0000001, fill = housing)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  xlab('Year') + ylab('Total number of housing units reported (in 1 million units)') +
  #scale_y_continuous(limits = c(0,170)) +
  theme_pub()

overall_sums <- bu_wui_cleaned %>%
  group_by(class, year) %>%
  summarise(ztrax = sum(residential_built_up, na.rm = TRUE),
            silvis = sum(house_units, na.rm = TRUE)) %>%
  gather(housing, value = c('ztrax', 'silvis'), -class, -year) %>%
  mutate(value = `c("ztrax", "silvis")`) %>% dplyr::select(-`c("ztrax", "silvis")`) %>%
  ggplot(aes(x = year, y = value*0.0000001, fill = housing)) +
  geom_bar(stat = 'identity', position=position_dodge(5)) +
  xlab('Year') + ylab('Difference of Ztrax and SILVIS (in 1 million units)') +
  theme_pub() +
  facet_wrap(~ class)

overall_sums2 <- bu_wui_cleaned %>%
  group_by(class, year) %>%
  summarise(ztrax = sum(residential_built_up, na.rm = TRUE),
            silvis = sum(house_units, na.rm = TRUE)) %>%
  mutate(pct_difference = ztrax-silvis) %>%
  ggplot(aes(x = year, y = pct_difference*0.0000001)) +
  geom_bar(stat = 'identity', position=position_dodge(.5)) +
  xlab('Year') + ylab('Difference of Ztrax and SILVIS (in 1 million units)') +
  #scale_y_continuous(limits = c(0,170)) +
  theme_pub() +
  # theme(legend.position = 'none') + 
  facet_wrap(~ class)

grid.arrange(overall_sums, overall_sums2, nrow = 1)



total <- as.data.frame(wui_209) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  filter(cause != 'Unk') %>%
  group_by() %>%
  summarise(total_costs = sum(costs))

pct_bu_totals <- as.data.frame(bu_cleaned) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by() %>%
  summarise(total_bu = sum(bu, na.rm = TRUE))

total_costs <- as.data.frame(wui_209) %>%
  filter(cause != 'Unk') %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(class) %>%
  summarise(costs = sum(costs)) %>%
  ungroup() %>%
  mutate(pct_costs = costs/total$total_costs*100) %>%
  ggplot(aes(x = class, y = pct_costs)) +
  geom_bar(stat = 'identity', position=position_dodge(.5)) +
  xlab('Class') + ylab('Percent of total costs (%)') +
  theme_pub() 

pct_bu <- as.data.frame(bu_cleaned) %>%
  filter(!(class_coarse %in% c('Other', 'Urban'))) %>%
  group_by(class, ignition) %>%
  summarise(sum_bu = sum(bu, na.rm = TRUE)) %>%
  mutate(pct_bu = (sum_bu/pct_bu_totals$total_bu)*100) %>%
  ggplot(aes(x = class, y = pct_bu)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  xlab('Class') + ylab('Percent of total threathened housing units (%)') +
  theme_pub() 

grid.arrange(total_costs, pct_bu, nrow = 1)  







