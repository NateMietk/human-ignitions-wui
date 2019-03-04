clean_mtbs <- function(mtbs_raw = mtbs, fpa_shp = fpa_fire, out_dir = accuracy_assessment_dir, mtbs_dir = mtbs_prefix, usa = usa_shp,
                       mtbs_shp = mtbs_fire, shp_filename, shp_filename_pts, rds_filename, buffer = FALSE, cores = parallel::detectCores()) {
  filtered <- function(df, sf_type) {
    # If there are multiple points per polygon, filter based on same fire year and within 25% of total fire sizes
    mtbs_remaining_filtered_id_only <- df %>%
      filter(MTBS_DISCOVERY_YEAR == DISCOVERY_YEAR) %>%
      filter(MTBS_DISCOVERY_MONTH == DISCOVERY_MONTH) %>%
      filter(MTBS_ACRES <= FIRE_SIZE+(FIRE_SIZE*0.15) & MTBS_ACRES >= FIRE_SIZE-(FIRE_SIZE*0.15)) %>%
      dplyr::select("MTBS_ID", 'FPA_ID')
    
    # Create a full dataframe of the outlier MTBS + FPA fires to be folded into the main df
    mtbs_remaining_filtered <- mtbs_remaining_filtered_id_only %>%
      dplyr::select('FPA_ID') %>%
      left_join(., as.data.frame(fpa_shp) %>% dplyr::select(-contains('geometry|geom')), by = 'FPA_ID') %>%
      dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, FIRE_SIZE, FIRE_SIZE_ha, FIRE_SIZE_km2, FIRE_SIZE_CL,
                    DISCOVERY_YEAR, DISCOVERY_DOY, DISCOVERY_MONTH, DISCOVERY_DAY, SEASONS, STATE, STAT_CAUSE_DESCR, IGNITION) %>%
      bind_cols(., as.data.frame(mtbs_remaining_filtered_id_only) %>% dplyr::select(-FPA_ID, -contains('geom'))) %>%
      merge(., as.data.frame(mtbs_raw) %>% dplyr::select(-geometry), by = 'MTBS_ID', all = FALSE) %>%
      dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, 
                    MTBS_ACRES, FIRE_SIZE, FIRE_SIZE_ha, FIRE_SIZE_km2, FIRE_SIZE_CL, MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, DISCOVERY_DOY, 
                    MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH, MTBS_DISCOVERY_DAY, DISCOVERY_DAY, SEASONS, STATE, STAT_CAUSE_DESCR, IGNITION) %>%
      st_cast(sf_type) %>%
      mutate(GEOMAC_ID = factor(NA_character_),
             GEOMAC_FIRE_NAME = factor(NA_character_),
             GEOMAC_DISCOVERY_YEAR = NA_integer_,
             GEOMAC_ACRES = NA_real_)
    
    if(st_geometry_type(mtbs_remaining_filtered)[[1]] == 'POINT') {

      mtbs_fpa_final <-  fpa_shp %>%
        dplyr::select(FPA_ID) %>%
        inner_join(., as.data.frame(mtbs_shp) %>% dplyr::select(-geom), by = 'FPA_ID') %>%
        mutate(GEOMAC_ID = factor(NA_character_),
               GEOMAC_FIRE_NAME = factor(NA_character_),
               GEOMAC_DISCOVERY_YEAR = NA_integer_,
               GEOMAC_ACRES = NA_real_) %>%
        dplyr::select(FPA_ID, MTBS_ID, GEOMAC_ID, 
                      GEOMAC_FIRE_NAME, MTBS_FIRE_NAME,
                      FIRE_SIZE, FIRE_SIZE_ha, FIRE_SIZE_km2, MTBS_ACRES, GEOMAC_ACRES, 
                      FIRE_SIZE_CL, DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                      MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH, 
                      DISCOVERY_DAY, MTBS_DISCOVERY_DAY, DISCOVERY_DOY,
                      SEASONS, STATE, STAT_CAUSE_DESCR, IGNITION) 
      
      mtbs_remaining_filtered <- mtbs_remaining_filtered %>%
          dplyr::select(FPA_ID, MTBS_ID, GEOMAC_ID, 
                        GEOMAC_FIRE_NAME, MTBS_FIRE_NAME,
                        FIRE_SIZE, FIRE_SIZE_ha, FIRE_SIZE_km2, MTBS_ACRES, GEOMAC_ACRES, 
                        FIRE_SIZE_CL, DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                        MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH, 
                        DISCOVERY_DAY, MTBS_DISCOVERY_DAY, DISCOVERY_DOY,
                        SEASONS, STATE, STAT_CAUSE_DESCR, IGNITION) 
      
      names(mtbs_remaining_filtered)[24]="geom"
      st_geometry(mtbs_remaining_filtered) = "geom"
      
      mtbs_fpa_final <- rbind(mtbs_fpa_final, mtbs_remaining_filtered)
        
      
    } else {
      mtbs_fpa_final <- as.data.frame(mtbs_shp) %>%
        full_join(., as.data.frame(mtbs_remaining_filtered) %>% dplyr::select(-contains('geometry'))) %>%
        st_as_sf() %>%
        dplyr::select(FPA_ID, MTBS_ID, GEOMAC_ID, 
                      GEOMAC_FIRE_NAME, MTBS_FIRE_NAME,
                      FIRE_SIZE, FIRE_SIZE_ha, FIRE_SIZE_km2, MTBS_ACRES, GEOMAC_ACRES, 
                      FIRE_SIZE_CL, DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                      MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH, 
                      DISCOVERY_DAY, MTBS_DISCOVERY_DAY, DISCOVERY_DOY,
                      SEASONS, STATE, STAT_CAUSE_DESCR, IGNITION) %>%
        lwgeom::st_make_valid(.)   
      
      names(mtbs_fpa_final)[24]="geom"
      st_geometry(mtbs_fpa_final) = "geom"
    }
    
    return(mtbs_fpa_final)
  }
  
  fpa_slim <- fpa_shp %>% 
    dplyr::select(FPA_ID, FIRE_SIZE, DISCOVERY_DAY, DISCOVERY_MONTH, DISCOVERY_YEAR)

  if(buffer == TRUE) {
    mtbs_raw <- mtbs_raw %>% 
      st_parallel(., st_buffer, n_cores =  cores, dist = 2600)
  }
  
  # After joining based on ID, find the FPA points within MTBS polygons
  mtbs_remaining_pts <- mtbs_raw %>% 
    anti_join(., as.data.frame(mtbs_shp), by = c('MTBS_ID', 'MTBS_FIRE_NAME', 'MTBS_DISCOVERY_DAY', 
                                                 'MTBS_DISCOVERY_MONTH', 'MTBS_DISCOVERY_YEAR', 'MTBS_ACRES')) %>%
    st_intersection(., fpa_slim) 
  
  mtbs_remaining <- mtbs_remaining_pts %>%
    as.data.frame(.) %>% dplyr::select(-geometry) %>%
    left_join(mtbs, ., by = c('MTBS_ID', 'MTBS_FIRE_NAME', 'MTBS_DISCOVERY_DAY', 'MTBS_DISCOVERY_MONTH', 'MTBS_DISCOVERY_YEAR', 'MTBS_ACRES'))
  
  mtbs_fpa_final_pts <- filtered(mtbs_remaining_pts, sf_type = 'POINT') 
  mtbs_fpa_final <- filtered(mtbs_remaining, sf_type = 'MULTIPOLYGON') 
  
  mtbs_count_df <- data.frame(as.data.frame(mtbs) %>% count(),
                              as.data.frame(mtbs_fpa_final) %>% count()) %>%
    dplyr::select(mtbs_inital_count = n,
                  mtbs_final_count = n.1) %>%
    mutate(pct_fpa_in_mtbs = mtbs_final_count/mtbs_inital_count)
  
  write_rds(mtbs_count_df, file.path(out_dir, rds_filename))
  st_write(mtbs_fpa_final, file.path(out_dir, shp_filename), delete_layer = TRUE)
  st_write(mtbs_fpa_final_pts, file.path(out_dir, shp_filename_pts), delete_layer = TRUE)
  return(mtbs_fpa_final)
}
