clean_mtbs <- function(fpa_shp = fpa_fire, out_dir = accuracy_assessment_dir, mtbs_dir = mtbs_prefix, usa = usa_shp,
                       mtbs_shp = mtbs_fire, shp_filename, rds_filename, buffer = FALSE) {
  # Find the percentage of FPA points are contained in the MTBS database
  mtbs <- st_read(dsn = mtbs_dir,
                  layer = "mtbs_perims_DD", quiet= TRUE) %>%
    st_transform(st_crs(usa)) %>%
    filter(Year >= 1992 & Year <= 2015) %>%
    mutate(MTBS_ID = Fire_ID,
           MTBS_FIRE_NAME = Fire_Name,
           MTBS_DISCOVERY_YEAR = Year,
           MTBS_DISCOVERY_DAY = StartDay,
           MTBS_DISCOVERY_MONTH = StartMonth,
           MTBS_ACRES = Acres) %>%
    dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR, MTBS_ACRES) 
  
  if(buffer == TRUE) {
    mtbs <- mtbs %>% 
      st_parallel(., st_buffer, n_cores =  parallel::detectCores(), dist = 1000)
  }
  
  # After joining based on ID, find the FPA points within MTBS polygons
  mtbs_remaining <- mtbs %>% 
    anti_join(., as.data.frame(mtbs_shp), by = c('MTBS_ID', 'MTBS_FIRE_NAME', 'MTBS_DISCOVERY_DAY', 'MTBS_DISCOVERY_MONTH', 'MTBS_DISCOVERY_YEAR', 'MTBS_ACRES')) %>%
    st_intersection(., fpa_shp %>% dplyr::select(FPA_ID, FIRE_SIZE, DISCOVERY_DAY, DISCOVERY_MONTH, DISCOVERY_YEAR)) %>%
    as.data.frame(.) %>% dplyr::select(-geometry) %>%
    left_join(mtbs, ., by = c('MTBS_ID', 'MTBS_FIRE_NAME', 'MTBS_DISCOVERY_DAY', 'MTBS_DISCOVERY_MONTH', 'MTBS_DISCOVERY_YEAR', 'MTBS_ACRES'))
  
  # If there are multiple points per polygon, filter based on same fire year and within 25% of total fire sizes
  mtbs_remaining_filtered_id_only <- mtbs_remaining %>%
    filter(MTBS_DISCOVERY_YEAR == DISCOVERY_YEAR) %>%
    filter(MTBS_DISCOVERY_MONTH == DISCOVERY_MONTH) %>%
    filter(MTBS_ACRES <= FIRE_SIZE+(FIRE_SIZE*0.15) & MTBS_ACRES >= FIRE_SIZE-(FIRE_SIZE*0.15)) %>%
    dplyr::select("MTBS_ID", 'FPA_ID')
  
  # Create a full dataframe of the outlier MTBS + FPA fires to be folded into the main df
  mtbs_remaining_filtered <- mtbs_remaining_filtered_id_only%>%
    dplyr::select('FPA_ID') %>%
    left_join(., as.data.frame(fpa_shp) %>% dplyr::select(-geom), by = 'FPA_ID') %>%
    dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                  DISCOVERY_YEAR, DISCOVERY_DOY, DISCOVERY_MONTH, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION) %>%
    bind_cols(., as.data.frame(mtbs_remaining_filtered_id_only) %>% dplyr::select(-FPA_ID, -geometry)) %>%
    merge(., as.data.frame(mtbs) %>% dplyr::select(-geometry), by = 'MTBS_ID', all = FALSE) %>%
    dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, MTBS_ACRES, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2, MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH, MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION) %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(GEOMAC_ID = factor(NA_character_),
           GEOMAC_FIRE_NAME = factor(NA_character_),
           GEOMAC_DISCOVERY_YEAR = NA_integer_,
           GEOMAC_ACRES = NA_real_)
  
  # Add these additional fires into the mtbs_fire points database
  mtbs_fpa_final <- as.data.frame(mtbs_shp) %>%
    full_join(., as.data.frame(mtbs_remaining_filtered) %>% dplyr::select(-geometry)) %>%
    st_as_sf() %>%
    dplyr::select(FPA_ID, MTBS_ID, GEOMAC_ID, 
                  GEOMAC_FIRE_NAME, MTBS_FIRE_NAME,
                  FPA_ACRES = FIRE_SIZE, MTBS_ACRES, GEOMAC_ACRES, 
                  FPA_DISCOVERY_YEAR = DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                  MTBS_DISCOVERY_MONTH, FPA_DISCOVERY_MONTH = DISCOVERY_MONTH, 
                  FPA_ = DISCOVERY_DAY, MTBS_DISCOVERY_DAY, FPA_DISCOVERY_DOY = DISCOVERY_DOY,
                  STATE, STAT_CAUSE_DESCR, IGNITION) %>%
    lwgeom::st_make_valid(.)
  
  mtbs_count_df <- data.frame(as.data.frame(mtbs_shp) %>% count(),
                              as.data.frame(mtbs_fpa_final) %>% count()) %>%
    dplyr::select(mtbs_inital_count = n,
                  mtbs_final_count = n.1) %>%
    mutate(pct_fpa_in_mtbs = mtbs_final_count/mtbs_inital_count)
  
  write_rds(mtbs_count_df, file.path(out_dir, rds_filename))
  st_write(mtbs_fpa_final, file.path(out_dir, shp_filename), delete_layer = TRUE)
  return(mtbs_fpa_final)
}
