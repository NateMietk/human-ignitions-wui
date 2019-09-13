library(xlsx)

fpa_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_wui_df.rds'))
fpa_bae_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_bae_wui_df.rds')) %>%
  left_join(., wuw_area, by = c('class', 'decadal'))
wui_209_df <- read_rds(file.path(rmarkdown_files, "wui_209_df.rds"))
bu_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))
bu_ics_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))

row_order_wclass <- c("fire_frequency", "wildfire_burned_area", "class_burned_area", 'fire_season_length', 'median_discovery_day',
                      'costs', 'build_up_count_0', 'build_up_count_250', 'build_up_count_500', 'build_up_count_1000',
                      'pct_frequency', 'pct_burned_area', 'pct_class_burned_area', 'pct_costs', 'pt_build_up_count_0',
                      'pt_build_up_count_250', 'pt_build_up_count_500', 'pt_build_up_count_1000')
raw_order_risky <- c('fire_frequency', 'wildfire_area', 'costs', 'risky_built_up_0', 'risky_built_up_250', 'risky_built_up_500', 'risky_built_up_1000')
row_order_for_class <- c("fire_frequency", "wildfire_burned_area", "class_burned_area", 'fire_season_length', 'median_discovery_day',
                         'costs', 'build_up_count_0', 'build_up_count_250', 'build_up_count_500', 'build_up_count_1000')
# Main Text
#### Table 3 -Totals across ignition and class ---------------------------------------------------------
# Totals across ignition AND WUI, VLD, and Wildlands only
if(!exists('table_1_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class_coarse, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class_coarse, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2),
              total_class_area = first(total_class_area)) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_class_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    mutate(ignition = cause) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class_coarse, ignition) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class_coarse, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t1_final <- t1 %>%
    left_join(., t2, by = c('class_coarse', 'ignition')) %>%
    left_join(., t3, by = c('class_coarse', 'ignition')) %>%
    left_join(., t4, by = c('class_coarse', 'ignition')) %>%
    unite(ignition_class, class_coarse, ignition) %>%
    gather(var, value, -ignition_class) %>% 
    spread(ignition_class, value) %>%
    mutate(pct_human_wui = round(`WUI_Human`/(`WUI_Human` + `WUI_Lightning`)*100,0),
           pct_human_vld = round(VLD_Human/(VLD_Human + VLD_Lightning)*100,0),
           pct_human_wildlands = round(Wildlands_Human/(Wildlands_Human + Wildlands_Lightning)*100,0)) %>%
    dplyr::select(var, `WUI_Human`, `WUI_Lightning`, pct_human_wui, 
                  VLD_Human, VLD_Lightning, pct_human_vld,
                  Wildlands_Human, Wildlands_Lightning, pct_human_wildlands) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(t1_final, file = file.path(figs_dir, "table_1_final.xlsx"), 
             sheetName = "1")
  system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
}


# Supplemental Tables
##### Table S1 - Regional by Fire Size Tables ---------------------------------------------------------
#### Table 1a - Fire Size  WUI
if(!exists('s1a_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(class_coarse == 'WUI') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% 
    filter(class == 'Interface WUI' & decadal == '2010' | class == 'Intermix WUI' & decadal == '2010') %>%
    group_by() %>%
    summarise(total_class_area = sum(total_class_area))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(class_coarse == 'WUI') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    filter(!is.na(fire_size)) %>%
    transform(fire_size = factor(fire_size, levels=c('Small', 'Large', 'Very Large'))) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(class_coarse == 'WUI') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%  
    mutate_if(is.numeric, funs(round(., 2)))
  
  t4 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(class_coarse == 'WUI') %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s1a_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t4, by = c('fire_size', 'region','ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, 
                  Small_Central_Human, Small_Central_Lightning, 
                  Small_West_Human, Small_West_Lightning,
                  Large_East_Human, Large_East_Lightning,
                  Large_Central_Human, Large_Central_Lightning,
                  Large_West_Human, Large_West_Lightning) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s5b_final, file = file.path(figs_dir, "supplemental_tables_raw_4_5.xlsx"),  
             sheetName = "s5b", append = TRUE)
}

#### Table 1b - Fire Size VLD 
if(!exists('s5d_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(class_coarse == 'VLD') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'VLD' & decadal == '2010')
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(class_coarse == 'VLD') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    filter(!is.na(fire_size)) %>%
    transform(fire_size = factor(fire_size, levels=c('Small', 'Large', 'Very Large'))) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(class_coarse == 'VLD') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%  
    mutate_if(is.numeric, funs(round(., 2)))
  
  t4 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(class_coarse == 'VLD') %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, region,  ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s1b_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t4, by = c('fire_size', 'region','ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, 
                  Small_Central_Human, Small_Central_Lightning, 
                  Small_West_Human, Small_West_Lightning,
                  Large_East_Human, Large_East_Lightning,
                  Large_Central_Human, Large_Central_Lightning,
                  Large_West_Human, Large_West_Lightning,
                  `Very Large_Central_Human`, `Very Large_Central_Lightning`,
                  `Very Large_West_Human`, `Very Large_West_Lightning`) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s1b_final, file = file.path(figs_dir, "supplemental_tables_raw_4_5.xlsx"),  
             sheetName = "s5d", append = TRUE)
}

#### Table 1c - Fire Size Wildlands 
if(!exists('s1c_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(class_coarse == 'Wildlands') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Wildlands' & decadal == '2010')
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(class_coarse == 'Wildlands') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    filter(!is.na(fire_size)) %>%
    transform(fire_size = factor(fire_size, levels=c('Small', 'Large', 'Very Large'))) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(class_coarse == 'Wildlands') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%  
    mutate_if(is.numeric, funs(round(., 2)))
  
  t4 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(class_coarse == 'Wildlands') %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, region, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s1c_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region', 'ignition')) %>%
    left_join(., t4, by = c('fire_size', 'region', 'ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, 
                  Small_Central_Human, Small_Central_Lightning, 
                  Small_West_Human, Small_West_Lightning,
                  Large_East_Human, Large_East_Lightning,
                  Large_Central_Human, Large_Central_Lightning, 
                  Large_West_Human, Large_West_Lightning, 
                  `Very Large_East_Human`, `Very Large_East_Lightning`, 
                  `Very Large_Central_Human`, `Very Large_Central_Lightning`, 
                  `Very Large_West_Human`, `Very Large_West_Lightning`) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s5e_final, file = file.path(figs_dir, "supplemental_tables_raw_4_5.xlsx"),  
             sheetName = "s5e", append = TRUE)
  system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
  
}


##### Table S2 - Seasonal Tables ---------------------------------------------------------
#### Table 2a - Seasonal  WUI
if(!exists('s2a_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(class_coarse == 'WUI') %>%
    group_by(seasons, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% 
    filter(class == 'Interface WUI' & decadal == '2010' | class == 'Intermix WUI' & decadal == '2010') %>%
    group_by() %>%
    summarise(total_class_area = sum(total_class_area))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(class_coarse == 'WUI') %>%
    group_by(seasons, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(class_coarse == 'WUI') %>%
    group_by(seasons, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%  
    mutate_if(is.numeric, funs(round(., 2)))
  
  t4 <- as_tibble(as.data.frame(wui_209_df)) %>%
    mutate(seasons = as.factor(classify_seasons(start_doy))) %>%
    filter(class_coarse == 'WUI') %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(seasons, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s2a_final <- t1 %>%
    left_join(., t2, by = c('seasons', 'ignition')) %>%
    left_join(., t3, by = c('seasons', 'ignition')) %>%
    left_join(., t4, by = c('seasons', 'ignition')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    dplyr::select(var, Spring_Human, Spring_Lightning, Summer_Human, Summer_Lightning,
                  Fall_Human, Fall_Lightning, Winter_Human, Winter_Lightning) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7b_final, file = file.path(figs_dir, "supplemental_tables_raw_6_8.xlsx"),  
             sheetName = "s7b", append = TRUE)
}

#### Table 2b - Seasonal VLD 
if(!exists('s2b_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    group_by(seasons, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'VLD' & decadal == '2010')
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    group_by(seasons, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    group_by(seasons, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%  
    mutate_if(is.numeric, funs(round(., 2)))
  
  t4 <- as_tibble(as.data.frame(wui_209_df)) %>%
    mutate(seasons = as.factor(classify_seasons(start_doy))) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(seasons, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s2b_final <- t1 %>%
    left_join(., t2, by = c('seasons', 'ignition')) %>%
    left_join(., t3, by = c('seasons', 'ignition')) %>%
    left_join(., t4, by = c('seasons', 'ignition')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
     dplyr::select(var, Spring_Human, Spring_Lightning, Summer_Human, Summer_Lightning,
                  Fall_Human, Fall_Lightning, Winter_Human, Winter_Lightning) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7d_final, file = file.path(figs_dir, "supplemental_tables_raw_6_8.xlsx"),  
             sheetName = "s7d", append = TRUE)
}

#### Table 2c - Seasonal Wildlands WUI
if(!exists('s2c_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    group_by(seasons, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Wildlands' & decadal == '2010')
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    group_by(seasons, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    group_by(seasons, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%  
    mutate_if(is.numeric, funs(round(., 2)))
  
  t4 <- as_tibble(as.data.frame(wui_209_df)) %>%
    mutate(seasons = as.factor(classify_seasons(start_doy))) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(seasons, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s2c_final <- t1 %>%
    left_join(., t2, by = c('seasons', 'ignition')) %>%
    left_join(., t3, by = c('seasons', 'ignition')) %>%
    left_join(., t4, by = c('seasons', 'ignition')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    dplyr::select(var, Spring_Human, Spring_Lightning, Summer_Human, Summer_Lightning,
                  Fall_Human, Fall_Lightning, Winter_Human, Winter_Lightning) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7e_final, file = file.path(figs_dir, "supplemental_tables_raw_6_8.xlsx"),  
             sheetName = "s7e", append = TRUE)
  
}

##### Table S3 - Risk Tables ---------------------------------------------------------
bu_ics_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_ics_complete_cleaned.rds'))
# Asses the risk of homes by ignition type AND WUI, VLD, and Wildlands only
if(!exists('s3_final')) {
  
  s3_final <- as.data.frame(bu_ics_complete_cleaned) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    filter(built_class != 'Non-Residential') %>%
    droplevels() %>%
    filter(cause != 'Unk') %>%
    mutate(at_risk = ifelse(build_up_count_no_zero_0 == 0 & build_up_count_no_zero_250 == 0 & 
                              build_up_count_no_zero_500 == 0 & build_up_count_no_zero_1000 == 0, "Not at risk", 'At risk')) %>% 
    group_by(class_coarse, cause, at_risk) %>%
    summarise(fire_frequency = n(),
              wildfire_area = sum(area_km2),
              costs = sum(costs),
              risky_built_up_0 = sum(build_up_count_no_zero_0),
              risky_built_up_250 = sum(build_up_count_no_zero_250),
              risky_built_up_500 = sum(build_up_count_no_zero_500),
              risky_built_up_1000 = sum(build_up_count_no_zero_1000)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    tidyr::unite(ignition_class, class_coarse, cause, at_risk) %>%
    tidyr::gather(var, value, -ignition_class) %>% 
    tidyr::spread(ignition_class, value) %>%
    dplyr::select(var, `WUI_Human_At risk`, `WUI_Human_Not at risk`, `WUI_Lightning_At risk`, `WUI_Lightning_Not at risk`,
                  `VLD_Human_At risk`, `VLD_Human_Not at risk`, `VLD_Lightning_At risk`, `VLD_Lightning_Not at risk`,
                  `Wildlands_Human_At risk`, `Wildlands_Human_Not at risk`, `Wildlands_Lightning_At risk`, `Wildlands_Lightning_Not at risk`) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(raw_order_risky, var))
  
  write.xlsx(s8b_final, file = file.path(figs_dir, "supplemental_tables_raw_6_8.xlsx"), 
             sheetName = "s8b", append = TRUE)
  system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
  
}


##### Table S4 - States Total across classes and ignition---------------------------------------------------------
if(!exists('s11_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class_coarse, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  tmp <- ungroup(wui_stusps_sum) %>% mutate(class_coarse = if_else(class == 'Interface WUI' | class == 'Intermix WUI', 'WUI', as.character(class))) %>%
    filter(year == '2010') %>% dplyr::select(-year, -class) %>%
    group_by(stusps, class_coarse) %>% summarise(total_stusps_area = sum(total_stusps_area))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class_coarse, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    left_join(., tmp, by = c('stusps', 'class_coarse')) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_stusps_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    mutate(ignition = cause) %>%
    group_by(stusps, class_coarse, ignition) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class_coarse, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s3_final <- t1 %>%
    left_join(., t2, by = c('class_coarse', 'stusps', 'ignition')) %>%
    left_join(., t3, by = c('class_coarse', 'stusps', 'ignition')) %>%
    left_join(., t4, by = c('class_coarse', 'stusps', 'ignition')) %>%
    tidyr::unite(ignition_class, class_coarse, ignition) %>%
    tidyr::gather(var, value, -stusps, -ignition_class) %>% 
    tidyr::spread(ignition_class, value) %>%
    dplyr::select(var, stusps, `WUI_Human`, `WUI_Lightning`,
                  VLD_Human, VLD_Lightning,
                  Wildlands_Human, Wildlands_Lightning) %>%
    slice(match(row_order_for_class, var)) %>%
    mutate_if(~ any(is.na(.x)),~ if_else(is.na(.x),0,.x))
  
  write.xlsx(as.data.frame(s11_final), file = file.path(figs_dir,"supplemental_tables_raw_9_13.xlsx"), 
             sheetName = "S11", append = TRUE)
}

##### Table S5 - Ecoregions Total across classes and ignition---------------------------------------------------------
if(!exists('s13_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    mutate(us_l3name = tolower(us_l3name),
           us_l3name = as.factor(capitalize(us_l3name))) %>%
    group_by(us_l3name, class_coarse, ignition) %>%    
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
   tmp <- ungroup(wui_ecol3name_sum) %>% 
    mutate(us_l3name = tolower(us_l3name),
           us_l3name = as.factor(capitalize(us_l3name)),
           class = as.factor(class),
           class_coarse = if_else(class == 'Interface WUI' | class == 'Intermix WUI', 'WUI', as.character(class))) %>%
    filter(year == '2010') %>% 
     filter(!(class_coarse %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
     group_by(us_l3name, class_coarse) %>% 
     summarise(total_ecol3name_area = sum(total_ecol3name_area))
    
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    mutate(us_l3name = tolower(us_l3name),
           us_l3name = as.factor(capitalize(us_l3name))) %>%
    group_by(us_l3name, class_coarse, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    left_join(., tmp, by = c('us_l3name', 'class_coarse')) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_ecol3name_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    ungroup() %>%
    mutate(class_coarse = as.factor(class_coarse))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    mutate(us_l3name = tolower(us_l3name),
           us_l3name = as.factor(capitalize(us_l3name)),
           ignition = as.factor(cause)) %>%
    group_by(us_l3name, class_coarse, ignition) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
    mutate(us_l3name = tolower(us_l3name),
           us_l3name = as.factor(capitalize(us_l3name))) %>%
    group_by(us_l3name, class_coarse, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s5_final <- t1 %>%
    left_join(., t2, by = c('class_coarse', 'us_l3name', 'ignition')) %>%
    left_join(., t3, by = c('class_coarse', 'us_l3name', 'ignition')) %>%
    left_join(., t4, by = c('class_coarse', 'us_l3name', 'ignition')) %>%
    tidyr::unite(ignition_class, class_coarse, ignition) %>%
    tidyr::gather(var, value, -us_l3name, -ignition_class) %>% 
    tidyr::spread(ignition_class, value) %>%
      dplyr::select(us_l3name, var, `WUI_Human`, `WUI_Lightning`,
                  VLD_Human, VLD_Lightning,
                  Wildlands_Human, Wildlands_Lightning) %>%
    slice(match(row_order_for_class, var)) %>%
    mutate_if(~ any(is.na(.x)),~ if_else(is.na(.x),0,.x))
  
  write.xlsx(as.data.frame(s13_final), file = file.path(figs_dir,"supplemental_tables_raw_9_13.xlsx"), 
             sheetName = "s13", append = TRUE)
  system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
  
}
