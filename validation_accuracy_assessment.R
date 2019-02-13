
# Find the percentage of FPA points are contained in the GeoMac database
if(!file.exists(file.path(accuracy_assessment_dir, 'geomac_fpa.gpkg'))) {
  geomac <- st_read(file.path(geomac_raw_dir, 'US_HIST_FIRE_PERIMTRS_DD83.gdb')) %>%
    st_transform(st_crs(usa_shp)) %>%
    filter(!(year_ %in% c('2016', '2017', '2018'))) %>%
    mutate(GEOMAC_ID = fire_num,
           GEOMAC_FIRE_NAME = fire_name,
           GEOMAC_DISCOVERY_YEAR = as.integer(as.character(year_)),
           GEOMAC_ACRES = acres,
           RADIUS = NA_real_) %>%
    dplyr::select(GEOMAC_ID, GEOMAC_FIRE_NAME, GEOMAC_DISCOVERY_YEAR, GEOMAC_ACRES, RADIUS)
  
  geomac_ids <- geomac %>%
    dplyr::select(GEOMAC_ID)
  # How many FPA points are located in the GeoMac perimeters?
  geomac_fpa <- geomac %>%
    st_intersection(., fpa_fire)
  
  # If there are multiple points per polygon, filter based on same fire year and within 25% of total fire sizes
  geomac_fpa_filtered <- geomac_fpa %>%
    dplyr::select(FPA_ID, GEOMAC_ID, GEOMAC_FIRE_NAME, GEOMAC_DISCOVERY_YEAR, GEOMAC_ACRES, RADIUS, ICS_209_INCIDENT_NUMBER, FIRE_YEAR, FIRE_SIZE) %>%
    filter(GEOMAC_DISCOVERY_YEAR == FIRE_YEAR) %>%
    filter(GEOMAC_ACRES <= FIRE_SIZE+(FIRE_SIZE*0.50) & GEOMAC_ACRES >= FIRE_SIZE-(FIRE_SIZE*0.50)) %>%
    dplyr::select('FPA_ID', 'GEOMAC_ID')
  
  fire_repeats <- as.data.frame(geomac_fpa_filtered) %>%
    group_by(GEOMAC_ID) %>%
    count() %>%
    arrange(desc(n))
  
  geomac_fpa_final <- as.data.frame(geomac_fpa_filtered) %>%
    dplyr::select(-Shape) %>%
    left_join(., fire_repeats, by = 'GEOMAC_ID') %>%
    left_join(geomac_ids, ., by = 'GEOMAC_ID') %>%
    filter(n <= 1) %>% dplyr::select(-n) %>%
    left_join(., as.data.frame(geomac_fpa) %>% dplyr::select(-Shape), by = c('FPA_ID', 'GEOMAC_ID')) %>%
    mutate(MTBS_ID = factor(NA_character_),
           MTBS_FIRE_NAME = factor(NA_character_),
           MTBS_DISCOVERY_YEAR = NA_integer_,
           MTBS_DISCOVERY_DAY = NA_integer_,
           MTBS_DISCOVERY_MONTH = NA_integer_,
           MTBS_ACRES = NA_real_,
           RADIUS = NA_real_) %>%
    st_cast("MULTIPOLYGON") %>%
    dplyr::select(FPA_ID, MTBS_ID, GEOMAC_ID, 
                  FPA_ACRES = FIRE_SIZE, MTBS_ACRES, GEOMAC_ACRES, 
                  FPA_DISCOVERY_YEAR = DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                  MTBS_DISCOVERY_MONTH, FPA_DISCOVERY_MONTH = DISCOVERY_MONTH, 
                  FPA_ = DISCOVERY_DAY, MTBS_DISCOVERY_DAY, FPA_DISCOVERY_DOY = DISCOVERY_DOY,
                  STATE, STAT_CAUSE_DESCR, IGNITION, RADIUS) %>%
    lwgeom::st_make_valid(.)
  
  geomac_inital_count <- as.data.frame(geomac) %>% count()
  geomac_final_count <- as.data.frame(geomac_fpa_final) %>% count()
  pct_fpa_in_geomac <- geomac_final_count/geomac_inital_count
  # 0.2843126 <- 5100/17938
  names(geomac_fpa_final)[19]="geom"
  st_geometry(geomac_fpa_final) = "geom"
  
  st_write(geomac_fpa_final, file.path(accuracy_assessment_dir, 'geomac_fpa.gpkg'), delete_layer = TRUE)
} else {
  geomac_fpa_final <- st_read(file.path(accuracy_assessment_dir, 'geomac_fpa.gpkg'))
}

if(!file.exists(file.path(accuracy_assessment_dir, 'mtbs_fpa.gpkg'))) {
  # Find the percentage of FPA points are contained in the MTBS database
  mtbs <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                  layer = "dissolve_mtbs_perims_1984-2015_DD_20170501", quiet= TRUE) %>%
    st_transform(st_crs(usa_shp)) %>%
    filter(Year >= 1992 & Year <= 2015) %>%
    mutate(MTBS_ID = Fire_ID,
           MTBS_FIRE_NAME = Fire_Name,
           MTBS_DISCOVERY_YEAR = Year,
           MTBS_DISCOVERY_DAY = StartDay,
           MTBS_DISCOVERY_MONTH = StartMonth,
           MTBS_ACRES = Acres,
           RADIUS = NA_real_) %>%
    dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR, MTBS_ACRES, RADIUS) 
  
  # After joining based on ID, find the FPA points within MTBS polygons
  mtbs_remaining <- mtbs %>% 
    anti_join(., as.data.frame(mtbs_fire), by = c('MTBS_ID', 'MTBS_FIRE_NAME', 'MTBS_DISCOVERY_DAY', 'MTBS_DISCOVERY_MONTH', 'MTBS_DISCOVERY_YEAR', 'MTBS_ACRES')) %>%
    st_intersection(., fpa_fire %>% dplyr::select(FPA_ID, FIRE_SIZE, DISCOVERY_DAY, DISCOVERY_MONTH, DISCOVERY_YEAR)) %>%
    as.data.frame(.) %>% dplyr::select(-geometry) %>%
    left_join(mtbs, ., by = c('MTBS_ID', 'MTBS_FIRE_NAME', 'MTBS_DISCOVERY_DAY', 'MTBS_DISCOVERY_MONTH', 'MTBS_DISCOVERY_YEAR', 'MTBS_ACRES'))
  
  # If there are multiple points per polygon, filter based on same fire year and within 25% of total fire sizes
  mtbs_remaining_filtered_id_only <- mtbs_remaining %>%
    filter(MTBS_DISCOVERY_YEAR == DISCOVERY_YEAR) %>%
    filter(MTBS_DISCOVERY_MONTH == DISCOVERY_MONTH) %>%
    filter(MTBS_ACRES <= FIRE_SIZE+(FIRE_SIZE*0.50) & MTBS_ACRES >= FIRE_SIZE-(FIRE_SIZE*0.50)) %>%
    dplyr::select("MTBS_ID", 'FPA_ID')
  
  # Create a full dataframe of the outlier MTBS + FPA fires to be folded into the main df
  mtbs_remaining_filtered <- mtbs_remaining_filtered_id_only%>%
    dplyr::select('FPA_ID') %>%
    left_join(., as.data.frame(fpa_fire) %>% dplyr::select(-geom), by = 'FPA_ID') %>%
    dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                  DISCOVERY_YEAR, DISCOVERY_DOY, DISCOVERY_MONTH, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION) %>%
    bind_cols(., as.data.frame(mtbs_remaining_filtered_id_only) %>% dplyr::select(-FPA_ID, -geometry)) %>%
    merge(., as.data.frame(mtbs) %>% dplyr::select(-geometry), by = 'MTBS_ID', all = FALSE) %>%
    dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, MTBS_ACRES, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2, MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH, MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION, RADIUS) %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(GEOMAC_ID = factor(NA_character_),
           GEOMAC_FIRE_NAME = factor(NA_character_),
           GEOMAC_DISCOVERY_YEAR = NA_integer_,
           GEOMAC_ACRES = NA_real_,
           RADIUS = NA_real_)
  
  # Add these additional fires into the mtbs_fire points database
  mtbs_fpa_final <- as.data.frame(mtbs_fire) %>%
    full_join(., as.data.frame(mtbs_remaining_filtered) %>% dplyr::select(-geometry)) %>%
    st_as_sf() %>%
    dplyr::select(FPA_ID, MTBS_ID, GEOMAC_ID, 
                  FPA_ACRES = FIRE_SIZE, MTBS_ACRES, GEOMAC_ACRES, 
                  FPA_DISCOVERY_YEAR = DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                  MTBS_DISCOVERY_MONTH, FPA_DISCOVERY_MONTH = DISCOVERY_MONTH, 
                  FPA_ = DISCOVERY_DAY, MTBS_DISCOVERY_DAY, FPA_DISCOVERY_DOY = DISCOVERY_DOY,
                  STATE, STAT_CAUSE_DESCR, IGNITION, RADIUS) %>%
    lwgeom::st_make_valid(.)
  
  mtbs_inital_count <- as.data.frame(mtbs) %>% count()
  mtbs_final_count <- as.data.frame(mtbs_fpa_final) %>% count()
  pct_fpa_in_mtbs <- mtbs_final_count/mtbs_inital_count
  # 0.5371031 <- 9337/17384
  st_write(mtbs_fpa_final, file.path(accuracy_assessment_dir, 'mtbs_fpa.gpkg'), delete_layer = TRUE)
  
} else {
  mtbs_fpa_final <- st_read(file.path(accuracy_assessment_dir, 'mtbs_fpa.gpkg')) 
}

if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac.gpkg"))) {
  fpa_mtbs_geomac <- do.call(rbind, list(geomac_fpa_final, mtbs_fpa_final)) %>%
    mutate(FIRE_SIZE_M2 = as.numeric(st_area(.)),
           RADIUS = sqrt(FIRE_SIZE_M2/pi))
  st_write(fpa_mtbs_geomac, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
} else {
  fpa_mtbs_geomac <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac.gpkg")) 
}

if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae.gpkg"))) {
  
  fpa_mtbs_geomac_bae <- fpa_fire %>%
    dplyr::select(FPA_ID, geom) %>%
    left_join(as.data.frame(fpa_mtbs_geomac) %>%
                dplyr::select(-geom), ., by = 'FPA_ID') %>%
    st_as_sf() %>%
    sf::st_transform(proj_ed) %>%
    st_buffer(., dist = .$RADIUS) %>%
    st_transform(proj_ea) %>%
    lwgeom::st_make_valid(.)
  
  st_write(fpa_mtbs_geomac_bae, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
} else {
  fpa_mtbs_geomac_bae <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae.gpkg")) 
}

if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_difference.gpkg"))) {
  buffer_perimeter_difference <- st_erase(fpa_mtbs_geomac_bae, fpa_mtbs_geomac) %>%
    mutate(DIF_FIRE_SIZE_M2 = as.numeric(st_area(.)))
  
  st_write(buffer_perimeter_difference, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_difference.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
} else {
  buffer_perimeter_difference <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_difference.gpkg")) 
}

# What is the percentage of FPA points that fall within GeoMac and MTBS polygons?
# inital_count <- geomac_inital_count + mtbs_inital_count
# final_count <- geomac_final_count + mtbs_final_count
# pct_fpa <- final_count/inital_count
# 0.4158315 <- 14688/35322

# What is the percentage of buffer areas that falls outside of the MTBS+GOEMAC polygons?
as.data.frame(buffer_perimeter_difference) %>%
  group_by() %>%
  summarise(FIRE_SIZE_M2 = sum(FIRE_SIZE_M2),
            DIF_FIRE_SIZE_M2 = sum(DIF_FIRE_SIZE_M2)) %>%
  mutate(accuracy = DIF_FIRE_SIZE_M2/FIRE_SIZE_M2)

# Does the burned area percentage change based on ignition type?
as.data.frame(buffer_perimeter_difference) %>%
  group_by(IGNITION) %>%
  summarise(FIRE_SIZE_M2 = sum(FIRE_SIZE_M2),
            DIF_FIRE_SIZE_M2 = sum(DIF_FIRE_SIZE_M2)) %>%
  mutate(accuracy = DIF_FIRE_SIZE_M2/FIRE_SIZE_M2)
