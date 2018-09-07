
library(tidyverse)
library(Hmisc)
library(kableExtra)
library(mblm)

fpa_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_wui_df.rds'))
fpa_bae_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_bae_wui_df.rds'))
wui_209_df <- read_rds(file.path(rmarkdown_files, "wui_209_df.rds"))
bu_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))
bu_ics_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))


# Results
# Key stats in `32% of all wildfires originated in the WUI, but 95% of burned area occurred beyond` -----------
#### How much did the WUI burn
as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  group_by(class) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE),
            total_class_area = first(total_class_area)) %>%
  mutate(pct_burn = (sum_burned_area/total_class_area)*100)

#### Average total yearly burned area (km2) across the CONUS
as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
  group_by(discovery_year) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE)) %>%
  group_by() %>%
  do(data.frame(rbind(smean.cl.boot(.$sum_burned_area, B = 10000)))) %>%
  mutate(total_burn_per_year = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)

#### Average class burned by each year
df1 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  group_by(discovery_year, class) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE),
            total_class_area = first(total_class_area)) %>%
  mutate(pct_yrly_burn = (sum_burned_area/total_class_area)*100) %>%
  group_by(class) %>%
  do(data.frame(rbind(smean.cl.boot(.$pct_yrly_burn, B = 10000)))) %>%
  mutate(avg_pct_burn_per_year = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)

as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  group_by(discovery_year, class) %>%
  summarise(sum_burned_area = sum(wui_area_km2, na.rm = TRUE),
            total_class_area = first(total_class_area)) %>%
  mutate(pct_yrly_burn = (sum_burned_area/total_class_area)*100) %>%
  group_by(class) %>%
  summarise(total_burned = sum(sum_burned_area),
            avg_burned_per_year = mean(sum_burned_area),
            total_pct_burn_per_year = sum(pct_yrly_burn)) %>%
  mutate(pct_burn_from_total = (total_burned/sum(total_burned))*100) %>%
  left_join(., df1, by = 'class')

#### Average wildfire size in each class - fine classes
as_tibble(as.data.frame(fpa_wui_df)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  group_by(class) %>%
  do(data.frame(rbind(smean.cl.boot(.$fire_size_km2, B = 10000)))) %>%
  mutate(avg_fire_size_km2 = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper) 

#### Average wildfire size in each class - coarse classes
as_tibble(as.data.frame(fpa_wui_df)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  group_by(class_coarse) %>%
  do(data.frame(rbind(smean.cl.boot(.$fire_size_km2, B = 10000)))) %>%
  mutate(avg_fire_size_km2 = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)  

#### Length of the wildfire season in each class
as_tibble(as.data.frame(fpa_wui_df)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  group_by(class) %>%
  summarise(fire_season_length = IQR(discovery_doy))







# Key stats in `In the WUI, humans started nearly all wildfires, doubled fire season length, and were responsible for most of the costs` ----
# Percent of WUI burned area by wildfire size
as_tibble(as.data.frame(fpa_wui_df)) %>%
  filter(class_coarse == 'WUI') %>%
  mutate(fire_size = case_when(
    fire_size_km2 < 4 ~ 'Small',
    fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
    fire_size_km2 >= 500 ~ 'Very Large', 
    TRUE ~ NA_character_ )) %>%
  group_by(fire_size, fire_size, ignition) %>%
  summarise(fire_frequency = n(),
            wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
            fire_season_length = IQR(discovery_doy)) %>%
  ungroup() %>%
  mutate(pct_burned = wildfire_burned_area/sum(wildfire_burned_area))

# Regional increase in burn area
as_tibble(as.data.frame(fpa_wui_df)) %>%
  filter(class_coarse == 'WUI') %>%
  mutate(fire_size = case_when(
    fire_size_km2 < 4 ~ 'Small',
    fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
    fire_size_km2 >= 500 ~ 'Very Large', 
    TRUE ~ NA_character_ )) %>%
  filter(region != 'Central' ) %>%
  filter(ignition == 'Human' ) %>%
  group_by(fire_size, region) %>%
  summarise(f_cnt = n(),
            wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE)) %>%
  tidyr::unite(region_size, fire_size, region) %>%
  tidyr::gather(var, value, -region_size) %>% 
  tidyr::spread(region_size, value) %>%
  mutate(pct_west = Large_West/Large_East,
         pct_east = Small_East/Small_West)

# Proportional increase of wildfire due to humans
as_tibble(as.data.frame(fpa_wui_df)) %>%
  filter(class_coarse == 'WUI') %>%
  filter(region != 'Central' ) %>%
  group_by(ignition, region) %>%
  summarise(f_cnt = n()) %>%
  tidyr::spread(ignition, f_cnt) %>%
  mutate(total_increase =  Human-Lightning, 
         pct_increase = Human/Lightning )

  
  
  
  

# Key stats `Human-started wildfires threatened 56 million residential homes within 1-km of fire perimeters` -----
# Average homes threatened per fire size
as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential') %>%
  mutate(fire_size_ha = fire_size_km2*100,
         fire_size = case_when(
           fire_size_ha >  0 & fire_size_ha < 100 ~ '0-100',
           fire_size_ha >= 100 & fire_size_ha < 200 ~ '100-200',
           fire_size_ha >= 200 & fire_size_ha < 400 ~ '200-400',
           fire_size_ha >= 400 & fire_size_ha < 1000 ~ '400-1k',
           fire_size_ha >= 1000 & fire_size_ha < 10000 ~ '1k-10k',
           fire_size_ha >= 10000 & fire_size_ha < 50000 ~ '10k-50k',
           fire_size_ha >= 50000 ~ '> 50k', TRUE ~ NA_character_ )) %>%
  group_by(fire_size) %>%
  do(data.frame(rbind(smean.cl.boot(.$build_up_count_no_zero_0, B = 10000)))) %>%
  mutate(average_bu_per_fire = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)

# Average cost per fire in the WUI
as_tibble(as.data.frame(wui_209_df)) %>%
  filter((class %in% c('Interface WUI', 'Intermix WUI'))) %>%
  filter(cause != 'Unk') %>% 
  group_by(start_year, cause) %>%
  summarise(freq = sum(area_km2),
            costs = sum(costs),
            cost_per_fire = costs/freq) %>%
  mutate(cause = as.factor(cause)) %>%
  group_by(cause) %>%
  do(data.frame(rbind(smean.cl.boot(.$cost_per_fire, B = 10000)))) %>%
  mutate(average_cost_per_fire = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)

mtbs_fitted_0 <- as_tibble(as.data.frame(wui_209_df)) %>%
  filter((class %in% c('Interface WUI', 'Intermix WUI'))) %>%
  droplevels() %>%
  group_by(start_year) %>%
  summarise(costs = sum(costs, na.rm = TRUE)) 

# Human cost per km2 in east vs west
as_tibble(as.data.frame(wui_209_df)) %>%
  filter((class %in% c('Interface WUI', 'Intermix WUI'))) %>%
  filter(cause != 'Unk') %>% 
  group_by(start_year, region, cause) %>%
  summarise(freq = sum(area_km2),
            costs = sum(costs),
            cost_per_fire = costs/freq) %>%
  mutate(cause = as.factor(cause)) %>%
  group_by(region, cause) %>%
  do(data.frame(rbind(smean.cl.boot(.$cost_per_fire, B = 10000)))) %>%
  mutate(average_cost_per_fire = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)

# Huamn vs lightning threa
as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  filter((class %in% c('Interface WUI', 'Intermix WUI'))) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(freq = sum(build_up_count_no_zero_0, na.rm = TRUE)) %>%
  group_by(class, ignition) %>%
  do(data.frame(rbind(smean.cl.boot(.$freq, B = 10000)))) %>%
  mutate(average_cost_per_fire = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)

as_tibble(as.data.frame(wui_209_df)) %>%
  filter(cause != 'Unk') %>% 
  group_by(start_year) %>%
  summarise(freq = sum(area_km2),
            costs = sum(costs),
            cost_per_fire = costs/freq) %>%
  group_by(region, cause) %>%
  do(data.frame(rbind(smean.cl.boot(.$cost_per_fire, B = 10000)))) %>%
  mutate(average_cost_per_fire = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)


# Key stats `The majority of suppression costs occurred when protecting homes outside the WUI` -----
# Number of homes threatened 
as_tibble(as.data.frame(bu_ics_cleaned)) %>%
  group_by(start_year) %>%
  summarise(bu = sum(build_up_count_no_zero, na.rm = TRUE)) %>%
  group_by() %>%
  summarise(bu = sum(bu, na.rm = TRUE)) 
  