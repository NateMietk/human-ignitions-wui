clean_geomac <- function(shp_in, fpa_shp = fpa_fire, out_dir = accuracy_assessment_dir,
                          shp_filename, rds_filename) {
  geomac_fpa <- shp_in %>%
    st_join(., fpa_shp)
  
  # If there are multiple points per polygon, filter based on same fire year and within 25% of total fire sizes
  geomac_fpa_filtered <- geomac_fpa %>%
    filter(GEOMAC_DISCOVERY_YEAR == FIRE_YEAR) %>%
    filter(GEOMAC_ACRES <= FIRE_SIZE+(FIRE_SIZE*0.02) & GEOMAC_ACRES >= FIRE_SIZE-(FIRE_SIZE*0.02)) %>%
    mutate(row_number = row_number(),
           MTBS_ID = factor(NA_character_),
           MTBS_FIRE_NAME = factor(NA_character_),
           MTBS_DISCOVERY_YEAR = NA_integer_,
           MTBS_DISCOVERY_DAY = NA_integer_,
           MTBS_DISCOVERY_MONTH = NA_integer_,
           MTBS_ACRES = NA_real_) %>%
    dplyr::select(row_number, FPA_ID, MTBS_ID, GEOMAC_ID, 
                  GEOMAC_FIRE_NAME, MTBS_FIRE_NAME,
                  FPA_ACRES = FIRE_SIZE, MTBS_ACRES, GEOMAC_ACRES, 
                  FPA_DISCOVERY_YEAR = DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                  MTBS_DISCOVERY_MONTH, FPA_DISCOVERY_MONTH = DISCOVERY_MONTH, 
                  FPA_ = DISCOVERY_DAY, MTBS_DISCOVERY_DAY, FPA_DISCOVERY_DOY = DISCOVERY_DOY,
                  STATE, STAT_CAUSE_DESCR, IGNITION)
  
  geomac_fpa_vals <- geomac_fpa_filtered %>%
    dplyr::select(row_number) %>%
    as.data.frame(.) %>%
    dplyr::select(-contains('Shape|geom')) %>%
    group_by(row_number) %>%
    count() %>%
    arrange(desc(n)) %>%
    filter(n >= 1)
  
  geomac_fpa_final <- geomac_fpa_vals %>% 
    dplyr::select(-n) %>%
    left_join(geomac_fpa_filtered, ., by = c('row_number')) %>%
    dplyr::select(-row_number) %>%
    lwgeom::st_make_valid(.)
  
  names(geomac_fpa_final)[20]="geom"
  st_geometry(geomac_fpa_final) = "geom"
  
  geomac_count_df <- data.frame(as.data.frame(geomac) %>% count(),
                              as.data.frame(geomac_fpa_final) %>% count()) %>%
    dplyr::select(geomac_inital_count = n,
                  geomac_final_count = n.1) %>%
    mutate(pct_fpa_in_geomac = geomac_final_count/geomac_inital_count)
  
  write_rds(geomac_count_df, file.path(out_dir, rds_filename))
  st_write(geomac_fpa_final, file.path(out_dir, shp_filename), delete_layer = TRUE)
  return(geomac_fpa_final)
}
