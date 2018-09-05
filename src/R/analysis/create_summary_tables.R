library(xlsx)

fpa_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_wui_df.rds'))
fpa_bae_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_bae_wui_df.rds'))
wui_209_df <- read_rds(file.path(rmarkdown_files, "wui_209_df.rds"))
bu_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))
bu_ics_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))

row_order_wclass <- c("fire_frequency", "wildfire_burned_area", "class_burned_area", 'fire_season_length', 'median_discovery_day',
                      'costs', 'build_up_count_0', 'build_up_count_250', 'build_up_count_500', 'build_up_count_1000',
                      'pct_frequency', 'pct_burned_area', 'pct_class_burned_area', 'pct_costs', 'pt_build_up_count_0',
                      'pt_build_up_count_250', 'pt_build_up_count_500', 'pt_build_up_count_1000')
raw_order_risky <- c('fire_frequency', 'wildfire_area', 'costs', 'risky_built_up')
row_order_for_class <- c("fire_frequency", "wildfire_burned_area", "class_burned_area", 'fire_season_length', 'median_discovery_day',
                         'costs', 'build_up_count_0', 'build_up_count_250', 'build_up_count_500', 'build_up_count_1000')

# Supplemental Tables
#### Table 1 - Total across classes  ---------------------------------------------------------
if(!exists('s1_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class) %>%
    summarise(class_burned_area = sum(wui_area_km2),
              total_class_area = first(total_class_area)) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_class_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s1_final <- t1 %>%
    left_join(., t2, by = 'class') %>%
    left_join(., t3, by = 'class') %>%
    left_join(., t4, by = 'class') %>%
    rownames_to_column()  %>%
    gather(var, value, -rowname) %>% 
    spread(rowname, value) 
  colnames(s1_final) <- s1_final[5, ] 
  s1_final <- s1_final[-5, ]  
  
  s1_final <- s1_final %>%
    slice(match(row_order_wclass, class))
  
  write.xlsx(s1_final, file = "supplemental_tables_raw_1_3.xlsx", 
             sheetName = "S1")
}

#### Table 2 - Totals across ignition ---------------------------------------------------------
# Totals across ignition regardless of class type
if(!exists('s2_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    group_by(ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    mutate(ignition = cause) %>%
    group_by(ignition) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    group_by(ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s2_final <- t1 %>%
    left_join(., t3, by = 'ignition') %>%
    left_join(., t4, by = 'ignition') %>%
    rownames_to_column()  %>%
    gather(var, value, -rowname) %>% 
    spread(rowname, value) 
  colnames(s2_final) <- s2_final[8, ] 
  s2_final <- s2_final[-8, ]  
  
  s2_final <- s2_final %>%
    slice(match(row_order_wclass, ignition))
  
  write.xlsx(s2_final, file = "supplemental_tables_raw_1_3.xlsx", 
             sheetName = "S2", append = TRUE)
}

#### Table 2a 
# Totals across ignition for classes of WUI, VLD, and Wildlands only
if(!exists('s2a_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2),
              total_class_area = first(total_class_area)) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_class_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    mutate(ignition = cause) %>%
    group_by(ignition) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    filter(built_class == 'Residential') %>%
    group_by(ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s2a_final <- t1 %>%
    left_join(., t3, by = 'ignition') %>%
    left_join(., t2, by = 'ignition') %>%
    left_join(., t4, by = 'ignition') %>%
    rownames_to_column()  %>%
    gather(var, value, -rowname) %>% 
    spread(rowname, value) 
  colnames(s2a_final) <- s2a_final[9, ] 
  s2a_final <- s2a_final[-9, ]  
  s2a_final <- s2a_final %>%
    mutate(ignition = as.factor(ignition)) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(pct_human_interface = round(Human/(Human + Lightning)*100,0)) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, ignition))
  
  write.xlsx(s2a_final, file = "supplemental_tables_raw_1_3.xlsx", 
             sheetName = "S2a", append = TRUE)
}

#### Table 3 -Totals across ignition and class ---------------------------------------------------------
# Totals across ignition AND WUI, VLD, and Wildlands only
if(!exists('s3_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2),
              total_class_area = first(total_class_area)) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_class_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    mutate(ignition = cause) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class, ignition) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(class, ignition) %>%
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
    left_join(., t2, by = c('class', 'ignition')) %>%
    left_join(., t3, by = c('class', 'ignition')) %>%
    left_join(., t4, by = c('class', 'ignition')) %>%
    unite(ignition_class, class, ignition) %>%
    gather(var, value, -ignition_class) %>% 
    spread(ignition_class, value) %>%
    mutate(pct_human_interface = round(`Interface WUI_Human`/(`Interface WUI_Human` + `Interface WUI_Lightning`)*100,0),
           pct_human_intermix = round(`Intermix WUI_Human`/(`Intermix WUI_Human` + `Intermix WUI_Lightning`)*100,0),
           pct_human_vld = round(VLD_Human/(VLD_Human + VLD_Lightning)*100,0),
           pct_human_wildlands = round(Wildlands_Human/(Wildlands_Human + Wildlands_Lightning)*100,0)) %>%
    dplyr::select(var, `Intermix WUI_Human`, `Intermix WUI_Lightning`, pct_human_intermix,
                  `Interface WUI_Human`, `Interface WUI_Lightning`, pct_human_interface, 
                  VLD_Human, VLD_Lightning, pct_human_vld,
                  Wildlands_Human, Wildlands_Lightning, pct_human_wildlands) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s3_final, file = "supplemental_tables_raw_1_3.xlsx", 
             sheetName = "S3", append = TRUE)
}

##### Table 4 - Risk Tables ---------------------------------------------------------
# Table 4a - CONUS Asses the risk of homes by ignition type regardless of class type
if(!exists('s4a_final')) {
  
  s4a_final <- as.data.frame(bu_ics_cleaned) %>%
    filter(built_class != 'Non-Residential') %>%
    droplevels() %>%
    filter(cause != 'Unk') %>%
    mutate(at_risk = ifelse(build_up_count_no_zero == 0, "Not at risk", 'At risk')) %>% 
    group_by(cause, at_risk) %>%
    summarise(fire_frequency = n(),
              wildfire_area = sum(area_km2),
              costs = sum(costs),
              risky_built_up = sum(build_up_count_no_zero)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    tidyr::unite(cause_risk, cause, at_risk) %>%
    tidyr::gather(var, value, -cause_risk) %>% 
    tidyr::spread(cause_risk, value) %>%
    mutate(pct_human_risky = round(`Human_At risk`/(`Human_At risk` + `Human_Not at risk`)*100,0)) %>%
    dplyr::select(var, `Human_At risk`, `Human_Not at risk`, `Lightning_At risk`, `Lightning_Not at risk`, pct_human_risky) %>%
    slice(match(raw_order_risky, var))
  
  write.xlsx(s4a_final, file = "supplemental_tables_raw_4_5.xlsx", 
             sheetName = "S4a", append = TRUE)
}

#### Table 4b
# Asses the risk of homes by ignition type AND WUI, VLD, and Wildlands only
if(!exists('s4b_final')) {
  
  s4b_final <- bu_ics_cleaned %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    filter(built_class != 'Non-Residential') %>%
    droplevels() %>%
    filter(cause != 'Unk') %>%
    mutate(at_risk = ifelse(build_up_count_no_zero == 0, "Not at risk", 'At risk')) %>% 
    group_by(class, cause, at_risk) %>%
    summarise(fire_frequency = n(),
              wildfire_area = sum(area_km2),
              costs = sum(costs),
              risky_built_up = sum(build_up_count_no_zero)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    tidyr::unite(ignition_class, class, cause, at_risk) %>%
    tidyr::gather(var, value, -ignition_class) %>% 
    tidyr::spread(ignition_class, value) %>%
    mutate(pct_human_interface_risky = round(`Interface WUI_Human_At risk`/(`Interface WUI_Human_At risk` + `Interface WUI_Human_Not at risk`)*100,0),
           pct_human_intermix_risky = round(`Intermix WUI_Human_At risk`/(`Intermix WUI_Human_At risk` + `Intermix WUI_Human_Not at risk`)*100,0),
           pct_human_vld_risky = round(`VLD_Human_At risk`/(`VLD_Human_At risk` + `VLD_Human_Not at risk`)*100,0),
           pct_human_wildlands_risky = round(`Wildlands_Human_At risk`/(`Wildlands_Human_At risk` + `Wildlands_Human_Not at risk`)*100,0)) %>%
    dplyr::select(var, `Intermix WUI_Human_At risk`, `Intermix WUI_Human_Not at risk`, `Intermix WUI_Lightning_At risk`, `Intermix WUI_Lightning_Not at risk`, pct_human_intermix_risky, 
                  `Interface WUI_Human_At risk`, `Interface WUI_Human_Not at risk`, `Interface WUI_Lightning_At risk`, `Interface WUI_Lightning_Not at risk`, pct_human_interface_risky,  
                  `VLD_Human_At risk`, `VLD_Human_Not at risk`, `VLD_Lightning_At risk`, `VLD_Lightning_Not at risk`, pct_human_vld_risky,
                  `Wildlands_Human_At risk`, `Wildlands_Human_Not at risk`, `Wildlands_Lightning_At risk`, `Wildlands_Lightning_Not at risk`, pct_human_wildlands_risky) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(raw_order_risky, var))
  
  write.xlsx(s4b_final, file = "supplemental_tables_raw_4_5.xlsx", 
             sheetName = "S4b", append = TRUE)
}

##### Table 5 - Fire Size Tables ---------------------------------------------------------
# Table 5a - CONUS
if(!exists('s5a_final')) {
  
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    ungroup() %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 3))) 
  
  t2 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
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
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s5a_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'ignition')) %>%
    tidyr::unite(size_class, fire_size, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_small_human = round(Small_Human/(Small_Human + Small_Lightning)*100,0),
           pct_large_human = round(Large_Human/(Large_Human + Large_Lightning)*100,0),
           pct_very_large_human = round(`Very Large_Human`/(`Very Large_Human` + `Very Large_Lightning`)*100,0)) %>%
    dplyr::select(var, Small_Human, Small_Lightning, pct_small_human, Large_Human, Large_Lightning, pct_large_human, `Very Large_Human`, `Very Large_Lightning`, pct_very_large_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s5a_final, file = "supplemental_tables_raw_4_5.xlsx", 
             sheetName = "S5a", append = TRUE)
}

#### Table 5b - Fire Size Intermix WUI
if(!exists('s5b_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Intermix WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    filter(!is.na(fire_size)) %>%
    transform(fire_size = factor(fire_size, levels=c('Small', 'Large', 'Very Large'))) %>%
    group_by(fire_size, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s5b_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'ignition')) %>%
    left_join(., t4, by = c('fire_size', 'ignition')) %>%
    tidyr::unite(size_class, fire_size, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_small_human = round(Small_Human/(Small_Human + Small_Lightning)*100,0),
           pct_large_human = round(Large_Human/(Large_Human + Large_Lightning)*100,0)) %>%
    dplyr::select(var, Small_Human, Small_Lightning, pct_small_human, Large_Human, Large_Lightning, pct_large_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s5b_final, file = "supplemental_tables_raw_4_5.xlsx", 
             sheetName = "S5b", append = TRUE)
}

#### Table 5c - Fire Size Interface WUI
if(!exists('s5c_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Interface WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    filter(!is.na(fire_size)) %>%
    transform(fire_size = factor(fire_size, levels=c('Small', 'Large', 'Very Large'))) %>%
    group_by(fire_size, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s5c_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'ignition')) %>%
    left_join(., t4, by = c('fire_size', 'ignition')) %>%
    tidyr::unite(size_class, fire_size, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_small_human = round(Small_Human/(Small_Human + Small_Lightning)*100,0),
           pct_large_human = round(Large_Human/(Large_Human + Large_Lightning)*100,0)) %>%
    dplyr::select(var, Small_Human, Small_Lightning, pct_small_human, Large_Human, Large_Lightning, pct_large_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s5c_final, file = "supplemental_tables_raw_4_5.xlsx", 
             sheetName = "S5c", append = TRUE)
}

#### Table 5d - Fire Size VLD WUI
if(!exists('s5d_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'VLD' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    filter(!is.na(fire_size)) %>%
    transform(fire_size = factor(fire_size, levels=c('Small', 'Large', 'Very Large'))) %>%
    group_by(fire_size, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s5d_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'ignition')) %>%
    left_join(., t4, by = c('fire_size', 'ignition')) %>%
    tidyr::unite(size_class, fire_size, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_small_human = round(Small_Human/(Small_Human + Small_Lightning)*100,0),
           pct_large_human = round(Large_Human/(Large_Human + Large_Lightning)*100,0),
           pct_very_large_human = round(`Very Large_Human`/(`Very Large_Human` + `Very Large_Lightning`)*100,0)) %>%
    dplyr::select(var, Small_Human, Small_Lightning, pct_small_human, Large_Human, Large_Lightning, pct_large_human, `Very Large_Human`, `Very Large_Lightning`, pct_very_large_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s5d_final, file = "supplemental_tables_raw_4_5.xlsx", 
             sheetName = "S5d", append = TRUE)
}

#### Table 5e - Fire Size Wildlands WUI
if(!exists('s5e_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Wildlands' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    filter(!is.na(fire_size)) %>%
    transform(fire_size = factor(fire_size, levels=c('Small', 'Large', 'Very Large'))) %>%
    group_by(fire_size, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
    mutate(fire_size = case_when(
      fire_size_km2 < 4 ~ 'Small',
      fire_size_km2 >= 4 & fire_size_km2 < 500 ~ 'Large',
      fire_size_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ )) %>%
    group_by(fire_size, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
    filter(cause != 'Unk') %>%
    mutate(fire_size = case_when(
      area_km2 < 4 ~ 'Small',
      area_km2 >= 4 & area_km2 < 500 ~ 'Large',
      area_km2 >= 500 ~ 'Very Large', 
      TRUE ~ NA_character_ ),
      ignition = cause) %>%
    group_by(fire_size, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s5e_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'ignition')) %>%
    left_join(., t4, by = c('fire_size', 'ignition')) %>%
    tidyr::unite(size_class, fire_size, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_small_human = round(Small_Human/(Small_Human + Small_Lightning)*100,0),
           pct_large_human = round(Large_Human/(Large_Human + Large_Lightning)*100,0),
           pct_very_large_human = round(`Very Large_Human`/(`Very Large_Human` + `Very Large_Lightning`)*100,0)) %>%
    dplyr::select(var, Small_Human, Small_Lightning, pct_small_human, Large_Human, Large_Lightning, pct_large_human, `Very Large_Human`, `Very Large_Lightning`, pct_very_large_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s5e_final, file = "supplemental_tables_raw_4_5.xlsx", 
             sheetName = "S5e", append = TRUE)
}

##### Table 6 - Regional Tables ---------------------------------------------------------
# Table 6a Regional CONUS
if(!exists('s6a_final')) {
  
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    group_by(region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    ungroup() %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 3))) 
  
  t2 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    group_by(region, ignition) %>%
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
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(region, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s6a_final <- t1 %>%
    left_join(., t2, by = c('ignition', 'region')) %>%
    left_join(., t3, by = c('ignition', 'region')) %>%
    tidyr::unite(size_class, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_east_human = round(East_Human/(East_Human + East_Lightning)*100,0),
           pct_central_human = round(Central_Human/(Central_Human + Central_Lightning)*100,0),
           pct_west_human = round(West_Human/(West_Human + West_Lightning)*100,0)) %>%
    dplyr::select(var, East_Human, East_Lightning, pct_east_human, Central_Human, Central_Lightning, pct_central_human, West_Human, West_Lightning, pct_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s6a_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S6a", append = TRUE)
}

#### Table 6b - Regional Intermix WUI
if(!exists('s6b_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Intermix WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(region, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s6b_final <- t1 %>%
    left_join(., t2, by = c('region', 'ignition')) %>%
    left_join(., t3, by = c('region', 'ignition')) %>%
    left_join(., t4, by = c('region', 'ignition')) %>%
    tidyr::unite(size_class, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_east_human = round(East_Human/(East_Human + East_Lightning)*100,0),
           pct_central_human = round(Central_Human/(Central_Human + Central_Lightning)*100,0),
           pct_west_human = round(West_Human/(West_Human + West_Lightning)*100,0)) %>%
    dplyr::select(var, East_Human, East_Lightning, pct_east_human, Central_Human, Central_Lightning, pct_central_human, West_Human, West_Lightning, pct_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s6b_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S6b", append = TRUE)
}

#### Table 6c - Regional Interface WUI
if(!exists('s6c_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Interface WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(region, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s6c_final <- t1 %>%
    left_join(., t2, by = c('region', 'ignition')) %>%
    left_join(., t3, by = c('region', 'ignition')) %>%
    left_join(., t4, by = c('region', 'ignition')) %>%
    tidyr::unite(size_class, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_east_human = round(East_Human/(East_Human + East_Lightning)*100,0),
           pct_central_human = round(Central_Human/(Central_Human + Central_Lightning)*100,0),
           pct_west_human = round(West_Human/(West_Human + West_Lightning)*100,0)) %>%
    dplyr::select(var, East_Human, East_Lightning, pct_east_human, Central_Human, Central_Lightning, pct_central_human, West_Human, West_Lightning, pct_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s6c_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S6c", append = TRUE)
}

#### Table 6d - Regional VLD WUI
if(!exists('s6d_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'VLD' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    group_by(region, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(region, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s6d_final <- t1 %>%
    left_join(., t2, by = c('region', 'ignition')) %>%
    left_join(., t3, by = c('region', 'ignition')) %>%
    left_join(., t4, by = c('region', 'ignition')) %>%
    tidyr::unite(size_class, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_east_human = round(East_Human/(East_Human + East_Lightning)*100,0),
           pct_central_human = round(Central_Human/(Central_Human + Central_Lightning)*100,0),
           pct_west_human = round(West_Human/(West_Human + West_Lightning)*100,0)) %>%
    dplyr::select(var, East_Human, East_Lightning, pct_east_human, Central_Human, Central_Lightning, pct_central_human, West_Human, West_Lightning, pct_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s6d_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S6d", append = TRUE)
}

#### Table 6e - Regional Wildlands WUI
if(!exists('s6e_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    group_by(region, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Wildlands' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    group_by(region, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    group_by(region, ignition) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(region, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s6e_final <- t1 %>%
    left_join(., t2, by = c('region', 'ignition')) %>%
    left_join(., t3, by = c('region', 'ignition')) %>%
    left_join(., t4, by = c('region', 'ignition')) %>%
    tidyr::unite(size_class, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_east_human = round(East_Human/(East_Human + East_Lightning)*100,0),
           pct_central_human = round(Central_Human/(Central_Human + Central_Lightning)*100,0),
           pct_west_human = round(West_Human/(West_Human + West_Lightning)*100,0)) %>%
    dplyr::select(var, East_Human, East_Lightning, pct_east_human, Central_Human, Central_Lightning, pct_central_human, West_Human, West_Lightning, pct_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s6e_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S6e", append = TRUE)
}


##### Table 7 - Seasonal Tables ---------------------------------------------------------
#### Table 7a Seasonal CONUS
if(!exists('s7a_final')) {
  
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    group_by(seasons, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    ungroup() %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 3))) 
  
  t2 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
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
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    mutate(seasons = as.factor(classify_seasons(start_doy))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(seasons, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s7a_final <- t1 %>%
    left_join(., t2, by = c('ignition', 'seasons')) %>%
    left_join(., t3, by = c('ignition', 'seasons')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_human_spring = round(Spring_Human/(Spring_Human+Spring_Lightning)*100,0),
           pct_human_summer = round(Summer_Human/(Summer_Human+Summer_Lightning)*100,0),
           pct_human_fall = round(Fall_Human/(Fall_Human+Fall_Lightning)*100,0),
           pct_human_winter = round(Winter_Human/(Winter_Human+Winter_Lightning)*100,0)) %>%
    dplyr::select(var, Spring_Human, Spring_Lightning, pct_human_spring, Summer_Human, Summer_Lightning, pct_human_summer,
                  Fall_Human, Fall_Lightning, pct_human_fall, Winter_Human, Winter_Lightning, pct_human_winter) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7a_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S7a", append = TRUE)
}

#### Table 7b - Seasonal Intermix WUI
if(!exists('s7b_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(seasons, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Intermix WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(seasons, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(seasons, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s7b_final <- t1 %>%
    left_join(., t2, by = c('seasons', 'ignition')) %>%
    left_join(., t3, by = c('seasons', 'ignition')) %>%
    left_join(., t4, by = c('seasons', 'ignition')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_human_spring = round(Spring_Human/(Spring_Human+Spring_Lightning)*100,0),
           pct_human_summer = round(Summer_Human/(Summer_Human+Summer_Lightning)*100,0),
           pct_human_fall = round(Fall_Human/(Fall_Human+Fall_Lightning)*100,0),
           pct_human_winter = round(Winter_Human/(Winter_Human+Winter_Lightning)*100,0)) %>%
    dplyr::select(var, Spring_Human, Spring_Lightning, pct_human_spring, Summer_Human, Summer_Lightning, pct_human_summer,
                  Fall_Human, Fall_Lightning, pct_human_fall, Winter_Human, Winter_Lightning, pct_human_winter) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7b_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S7b", append = TRUE)
}

#### Table 7c - Seasonal Interface WUI
if(!exists('s7c_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    group_by(seasons, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2, na.rm = TRUE),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100)
  
  tmp <- wuw_area %>% filter(class == 'Interface WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    group_by(seasons, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    mutate(pct_class_burned_area = class_burned_area/(tmp$total_class_area)*100)
  
  t3 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    filter(cause != 'Unk') %>%
    mutate(ignition = cause) %>%
    group_by(seasons, ignition) %>%
    summarise(costs = sum(costs)) %>%
    ungroup() %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s7c_final <- t1 %>%
    left_join(., t2, by = c('seasons', 'ignition')) %>%
    left_join(., t3, by = c('seasons', 'ignition')) %>%
    left_join(., t4, by = c('seasons', 'ignition')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_human_spring = round(Spring_Human/(Spring_Human+Spring_Lightning)*100,0),
           pct_human_summer = round(Summer_Human/(Summer_Human+Summer_Lightning)*100,0),
           pct_human_fall = round(Fall_Human/(Fall_Human+Fall_Lightning)*100,0),
           pct_human_winter = round(Winter_Human/(Winter_Human+Winter_Lightning)*100,0)) %>%
    dplyr::select(var, Spring_Human, Spring_Lightning, pct_human_spring, Summer_Human, Summer_Lightning, pct_human_summer,
                  Fall_Human, Fall_Lightning, pct_human_fall, Winter_Human, Winter_Lightning, pct_human_winter) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7c_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S7c", append = TRUE)
}

#### Table 7d - Seasonal VLD WUI
if(!exists('s7d_final')) {
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
  
  tmp <- wuw_area %>% filter(class == 'VLD' & year == 2010)
  
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
  
  s7d_final <- t1 %>%
    left_join(., t2, by = c('seasons', 'ignition')) %>%
    left_join(., t3, by = c('seasons', 'ignition')) %>%
    left_join(., t4, by = c('seasons', 'ignition')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_human_spring = round(Spring_Human/(Spring_Human+Spring_Lightning)*100,0),
           pct_human_summer = round(Summer_Human/(Summer_Human+Summer_Lightning)*100,0),
           pct_human_fall = round(Fall_Human/(Fall_Human+Fall_Lightning)*100,0),
           pct_human_winter = round(Winter_Human/(Winter_Human+Winter_Lightning)*100,0)) %>%
    dplyr::select(var, Spring_Human, Spring_Lightning, pct_human_spring, Summer_Human, Summer_Lightning, pct_human_summer,
                  Fall_Human, Fall_Lightning, pct_human_fall, Winter_Human, Winter_Lightning, pct_human_winter) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7d_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S7d", append = TRUE)
}

#### Table 7e - Seasonal Wildlands WUI
if(!exists('s7e_final')) {
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
  
  tmp <- wuw_area %>% filter(class == 'Wildlands' & year == 2010)
  
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
  
  s7e_final <- t1 %>%
    left_join(., t2, by = c('seasons', 'ignition')) %>%
    left_join(., t3, by = c('seasons', 'ignition')) %>%
    left_join(., t4, by = c('seasons', 'ignition')) %>%
    tidyr::unite(size_class, seasons, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_human_spring = round(Spring_Human/(Spring_Human+Spring_Lightning)*100,0),
           pct_human_summer = round(Summer_Human/(Summer_Human+Summer_Lightning)*100,0),
           pct_human_fall = round(Fall_Human/(Fall_Human+Fall_Lightning)*100,0),
           pct_human_winter = round(Winter_Human/(Winter_Human+Winter_Lightning)*100,0)) %>%
    dplyr::select(var, Spring_Human, Spring_Lightning, pct_human_spring, Summer_Human, Summer_Lightning, pct_human_summer,
                  Fall_Human, Fall_Lightning, pct_human_fall, Winter_Human, Winter_Lightning, pct_human_winter) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s7e_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "S7e", append = TRUE)
}

##### Table 8 - Regional by Fire Size Tables ---------------------------------------------------------
#### Table 8a
if(!exists('s8a_final')) {
  
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
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
    ungroup() %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 3))) 
  
  t2 <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
    filter(built_class == 'Residential') %>%
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
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
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
  
  s8a_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region','ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_Small_east_human = round(Small_East_Human/(Small_East_Human + Small_East_Lightning)*100,0),
           pct_Small_central_human = round(Small_Central_Human/(Small_Central_Human + Small_Central_Lightning)*100,0),
           pct_Small_west_human = round(Small_West_Human/(Small_West_Human + Small_West_Lightning)*100,0),
           pct_Large_east_human = round(Large_East_Human/(Large_East_Human + Large_East_Lightning)*100,0),
           pct_Large_central_human = round(Large_Central_Human/(Large_Central_Human + Large_Central_Lightning)*100,0),
           pct_Large_west_human = round(Large_West_Human/(Large_West_Human + Large_West_Lightning)*100,0),
           pct_VeryLarge_east_human = round(`Very Large_East_Human`/(`Very Large_East_Human` + `Very Large_East_Lightning`)*100,0),
           pct_VeryLarge_central_human = round(`Very Large_Central_Human`/(`Very Large_Central_Human` + `Very Large_Central_Lightning`)*100,0),
           pct_VeryLarge_west_human = round(`Very Large_West_Human`/(`Very Large_West_Human` + `Very Large_West_Lightning`)*100,0)) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, pct_Small_east_human, 
                  Small_Central_Human, Small_Central_Lightning, pct_Small_central_human, 
                  Small_West_Human, Small_West_Lightning, pct_Small_west_human,
                  Large_East_Human, Large_East_Lightning, pct_Large_east_human,
                  Large_Central_Human, Large_Central_Lightning, pct_Large_central_human,
                  Large_West_Human, Large_West_Lightning, pct_Large_west_human,
                  `Very Large_East_Human`, `Very Large_East_Lightning`, pct_VeryLarge_east_human,
                  `Very Large_Central_Human`, `Very Large_Central_Lightning`, pct_VeryLarge_central_human,
                  `Very Large_West_Human`, `Very Large_West_Lightning`, pct_VeryLarge_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s8a_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "s8a", append = TRUE)
}

#### Table 8b - Fire Size Intermix WUI
if(!exists('s8b_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
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
  
  tmp <- wuw_area %>% filter(class == 'Intermix WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Intermix WUI')) %>%
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
  
  s8b_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t4, by = c('fire_size', 'region','ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_Small_east_human = round(Small_East_Human/(Small_East_Human + Small_East_Lightning)*100,0),
           pct_Small_central_human = round(Small_Central_Human/(Small_Central_Human + Small_Central_Lightning)*100,0),
           pct_Small_west_human = round(Small_West_Human/(Small_West_Human + Small_West_Lightning)*100,0),
           pct_Large_east_human = round(Large_East_Human/(Large_East_Human + 0)*100,0),
           pct_Large_central_human = round(Large_Central_Human/(Large_Central_Human + 0)*100,0),
           pct_Large_west_human = round(Large_West_Human/(Large_West_Human + Large_West_Lightning)*100,0)) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, pct_Small_east_human, 
                  Small_Central_Human, Small_Central_Lightning, pct_Small_central_human, 
                  Small_West_Human, Small_West_Lightning, pct_Small_west_human,
                  Large_East_Human, pct_Large_east_human,
                  Large_Central_Human, pct_Large_central_human,
                  Large_West_Human, Large_West_Lightning, pct_Large_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s8b_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "s8b", append = TRUE)
}

#### Table 8c - Fire Size Interface WUI
if(!exists('s8c_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
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
  
  tmp <- wuw_area %>% filter(class == 'Interface WUI' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
    transform(class = factor(class, levels='Interface WUI')) %>%
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
  
  s8c_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t4, by = c('fire_size', 'region','ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_Small_east_human = round(Small_East_Human/(Small_East_Human + Small_East_Lightning)*100,0),
           pct_Small_central_human = round(Small_Central_Human/(Small_Central_Human + Small_Central_Lightning)*100,0),
           pct_Small_west_human = round(Small_West_Human/(Small_West_Human + Small_West_Lightning)*100,0),
           pct_Large_east_human = round(Large_East_Human/(Large_East_Human + Large_East_Lightning)*100,0),
           pct_Large_central_human = round(Large_Central_Human/(Large_Central_Human + Large_Central_Lightning)*100,0),
           pct_Large_west_human = round(Large_West_Human/(Large_West_Human + Large_West_Lightning)*100,0)) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, pct_Small_east_human, 
                  Small_Central_Human, Small_Central_Lightning, pct_Small_central_human, 
                  Small_West_Human, Small_West_Lightning, pct_Small_west_human,
                  Large_East_Human, Large_East_Lightning, pct_Large_east_human,
                  Large_Central_Human, Large_Central_Lightning, pct_Large_central_human,
                  Large_West_Human, Large_West_Lightning, pct_Large_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s8c_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "s8c", append = TRUE)
}

#### Table 8d - Fire Size VLD WUI
if(!exists('s8d_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
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
  
  tmp <- wuw_area %>% filter(class == 'VLD' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'Wildlands'))) %>%
    transform(class = factor(class, levels='VLD')) %>%
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
  
  s8d_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region','ignition')) %>%
    left_join(., t4, by = c('fire_size', 'region','ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_Small_east_human = round(Small_East_Human/(Small_East_Human + Small_East_Lightning)*100,0),
           pct_Small_central_human = round(Small_Central_Human/(Small_Central_Human + Small_Central_Lightning)*100,0),
           pct_Small_west_human = round(Small_West_Human/(Small_West_Human + Small_West_Lightning)*100,0),
           pct_Large_east_human = round(Large_East_Human/(Large_East_Human + Large_East_Lightning)*100,0),
           pct_Large_central_human = round(Large_Central_Human/(Large_Central_Human + Large_Central_Lightning)*100,0),
           pct_Large_west_human = round(Large_West_Human/(Large_West_Human + Large_West_Lightning)*100,0),
           pct_VeryLarge_central_human = round(`Very Large_Central_Human`/(`Very Large_Central_Human` + `Very Large_Central_Lightning`)*100,0),
           pct_VeryLarge_west_human = round(`Very Large_West_Human`/(`Very Large_West_Human` + `Very Large_West_Lightning`)*100,0)) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, pct_Small_east_human, 
                  Small_Central_Human, Small_Central_Lightning, pct_Small_central_human, 
                  Small_West_Human, Small_West_Lightning, pct_Small_west_human,
                  Large_East_Human, Large_East_Lightning, pct_Large_east_human,
                  Large_Central_Human, Large_Central_Lightning, pct_Large_central_human,
                  Large_West_Human, Large_West_Lightning, pct_Large_west_human,
                  `Very Large_Central_Human`, `Very Large_Central_Lightning`, pct_VeryLarge_central_human,
                  `Very Large_West_Human`, `Very Large_West_Lightning`, pct_VeryLarge_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s8d_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "s8d", append = TRUE)
}

#### Table 8e - Fire Size Wildlands WUI
if(!exists('s8e_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
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
  
  tmp <- wuw_area %>% filter(class == 'Wildlands' & year == 2010)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
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
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'Intermix WUI', 'Interface WUI', 'VLD'))) %>%
    transform(class = factor(class, levels='Wildlands')) %>%
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
  
  s8e_final <- t1 %>%
    left_join(., t2, by = c('fire_size', 'region', 'ignition')) %>%
    left_join(., t3, by = c('fire_size', 'region', 'ignition')) %>%
    left_join(., t4, by = c('fire_size', 'region', 'ignition')) %>%
    tidyr::unite(size_class, fire_size, region, ignition) %>%
    tidyr::gather(var, value, -size_class) %>% 
    tidyr::spread(size_class, value) %>%
    mutate(pct_Small_east_human = round(Small_East_Human/(Small_East_Human + Small_East_Lightning)*100,0),
           pct_Small_central_human = round(Small_Central_Human/(Small_Central_Human + Small_Central_Lightning)*100,0),
           pct_Small_west_human = round(Small_West_Human/(Small_West_Human + Small_West_Lightning)*100,0),
           pct_Large_east_human = round(Large_East_Human/(Large_East_Human + Large_East_Lightning)*100,0),
           pct_Large_central_human = round(Large_Central_Human/(Large_Central_Human + Large_Central_Lightning)*100,0),
           pct_Large_west_human = round(Large_West_Human/(Large_West_Human + Large_West_Lightning)*100,0),
           pct_VeryLarge_east_human = round(`Very Large_East_Human`/(`Very Large_East_Human` + `Very Large_East_Lightning`)*100,0),
           pct_VeryLarge_central_human = round(`Very Large_Central_Human`/(`Very Large_Central_Human` + `Very Large_Central_Lightning`)*100,0),
           pct_VeryLarge_west_human = round(`Very Large_West_Human`/(`Very Large_West_Human` + `Very Large_West_Lightning`)*100,0)) %>%
    dplyr::select(var, Small_East_Human, Small_East_Lightning, pct_Small_east_human, 
                  Small_Central_Human, Small_Central_Lightning, pct_Small_central_human, 
                  Small_West_Human, Small_West_Lightning, pct_Small_west_human,
                  Large_East_Human, Large_East_Lightning, pct_Large_east_human,
                  Large_Central_Human, Large_Central_Lightning, pct_Large_central_human,
                  Large_West_Human, Large_West_Lightning, pct_Large_west_human,
                  `Very Large_East_Human`, `Very Large_East_Lightning`, pct_VeryLarge_east_human,
                  `Very Large_Central_Human`, `Very Large_Central_Lightning`, pct_VeryLarge_central_human,
                  `Very Large_West_Human`, `Very Large_West_Lightning`, pct_VeryLarge_west_human) %>%
    mutate_if(is.numeric, funs(as.character(signif(., 9)))) %>%
    slice(match(row_order_wclass, var))
  
  write.xlsx(s8e_final, file = "supplemental_tables_raw_6_8.xlsx", 
             sheetName = "s8e", append = TRUE)
}

##### Table 9 - States Total across classes ---------------------------------------------------------
if(!exists('s9_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class) %>%
    summarise(class_burned_area = sum(wui_area_km2),
              total_class_area = first(total_class_area)) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_class_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s9_final <- t1 %>%
    left_join(., t2, by = c('class', 'stusps')) %>%
    left_join(., t3, by = c('class', 'stusps')) %>%
    left_join(., t4, by = c('class', 'stusps')) %>%
    tidyr::gather(var, value, -stusps, -class) %>% 
    tidyr::spread(class, value) %>%
    slice(match(row_order_for_class, var)) %>%
    mutate_if(~ any(is.na(.x)),~ if_else(is.na(.x),0,.x))
  
  write.xlsx(as.data.frame(s9_final), file = "supplemental_tables_raw_9_12.xlsx", 
             sheetName = "S9", append = TRUE)
}

##### Table 10 - States Total across classes and ignition---------------------------------------------------------
if(!exists('s10_final')) {
  t1 <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class, ignition) %>%
    summarise(fire_frequency = n(),
              wildfire_burned_area = sum(fire_size_km2),
              fire_season_length = IQR(discovery_doy),
              median_discovery_day = median(discovery_doy)) %>%
    mutate(pct_frequency = fire_frequency/sum(fire_frequency)*100,
           pct_burned_area = wildfire_burned_area/sum(wildfire_burned_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  tmp <- ungroup(wui_stusps_sum) %>% filter(year == 2010) %>% dplyr::select(-year)
  
  t2 <- as_tibble(as.data.frame(fpa_bae_wui_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class, ignition) %>%
    summarise(class_burned_area = sum(wui_area_km2)) %>%
    left_join(., tmp, by = c('stusps', 'class')) %>%
    mutate(pct_class_burned_area = (class_burned_area/total_stusps_area)*100) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  
  t3 <- as_tibble(as.data.frame(wui_209_df)) %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    mutate(ignition = cause) %>%
    group_by(stusps, class, ignition) %>%
    summarise(costs = sum(costs)) %>%
    mutate(pct_costs = (costs/sum(costs))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  t4 <- as.data.frame(bu_complete_cleaned) %>%
    filter(built_class == 'Residential') %>%
    filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
    transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
    group_by(stusps, class, ignition) %>%
    summarise(build_up_count_0 = sum(build_up_count_no_zero_0, na.rm = TRUE),
              build_up_count_250 = sum(build_up_count_no_zero_250, na.rm = TRUE),
              build_up_count_500 = sum(build_up_count_no_zero_500, na.rm = TRUE),
              build_up_count_1000 = sum(build_up_count_no_zero_1000, na.rm = TRUE)) %>%
    mutate(pt_build_up_count_0 = (build_up_count_0/sum(build_up_count_0))*100,
           pt_build_up_count_250 = (build_up_count_250/sum(build_up_count_250))*100,
           pt_build_up_count_500 = (build_up_count_500/sum(build_up_count_500))*100,
           pt_build_up_count_1000 = (build_up_count_1000/sum(build_up_count_1000))*100) %>%
    mutate_if(is.numeric, funs(round(., 2))) 
  
  s10_final <- t1 %>%
    left_join(., t2, by = c('class', 'stusps', 'ignition')) %>%
    left_join(., t3, by = c('class', 'stusps', 'ignition')) %>%
    left_join(., t4, by = c('class', 'stusps', 'ignition')) %>%
    tidyr::unite(ignition_class, class, ignition) %>%
    tidyr::gather(var, value, -stusps, -ignition_class) %>% 
    tidyr::spread(ignition_class, value) %>%
    mutate(pct_human_interface = round(`Interface WUI_Human`/(`Interface WUI_Human` + `Interface WUI_Lightning`)*100,0),
           pct_human_intermix = round(`Intermix WUI_Human`/(`Intermix WUI_Human` + `Intermix WUI_Lightning`)*100,0),
           pct_human_vld = round(VLD_Human/(VLD_Human + VLD_Lightning)*100,0),
           pct_human_wildlands = round(Wildlands_Human/(Wildlands_Human + Wildlands_Lightning)*100,0)) %>%
    dplyr::select(var, `Intermix WUI_Human`, `Intermix WUI_Lightning`, pct_human_intermix,
                  `Interface WUI_Human`, `Interface WUI_Lightning`, pct_human_interface, 
                  VLD_Human, VLD_Lightning, pct_human_vld,
                  Wildlands_Human, Wildlands_Lightning, pct_human_wildlands) %>%
    slice(match(row_order_for_class, var)) %>%
    mutate_if(~ any(is.na(.x)),~ if_else(is.na(.x),0,.x))
  
  write.xlsx(as.data.frame(s10_final), file = "supplemental_tables_raw_9_12.xlsx", 
             sheetName = "S10", append = TRUE)
}

##### Table 11 - Total Class totals per state ---------------------------------------------------------
if(!exists('wui_class_area_tbl')) {

  s11_final <- as.data.frame(wui_class_area) %>%
    group_by(stusps, class10) %>%
    summarise(total_class_area = sum(total_class_area)) %>%
    ungroup() %>%
    mutate(State = stusps,
           Class = class10,
           Area_km2 = total_class_area) %>%
    dplyr::select(State, Class, Area_km2) %>%
    arrange(State, factor(Class, levels = c("High Urban", "Med Urban", "Low Urban", 'Intermix WUI', 
                                     'Interface WUI', 'VLD', 'Wildlands', 'Other'))) 
  
  write.xlsx(as.data.frame(s11_final), file = "supplemental_tables_raw_9_12.xlsx", 
             sheetName = "S11", append = TRUE)
}

##### Table 12 - Total Class totals per level 3 ecoregion ---------------------------------------------------------
if(!exists('wui_class_area_tbl')) {
  ecored_prep <- as.data.frame(ecoreg_plain) %>%
    dplyr::select(us_l3name, na_l1name)
  
  s12_final <- as.data.frame(wui_class_area) %>%
    left_join(., ecored_prep, by = 'us_l3name') %>%
    group_by(na_l1name, us_l3name, class10) %>%
    summarise(total_class_area = sum(total_class_area)) %>%
    ungroup() %>%
    mutate(Lvl1_Ecopregion = na_l1name,
           Lvl3_Ecoregion = us_l3name,
           Class = class10,
           Area_km2 = total_class_area) %>%
    dplyr::select(Lvl1_Ecopregion, Lvl3_Ecoregion, Class, Area_km2) %>%
    arrange(Lvl3_Ecoregion, factor(Class, levels = c("High Urban", "Med Urban", "Low Urban", 'Intermix WUI', 
                                            'Interface WUI', 'VLD', 'Wildlands', 'Other'))) 
  
  write.xlsx(as.data.frame(s12_final), file = "supplemental_tables_raw_9_12.xlsx", 
             sheetName = "S12", append = TRUE)
}





