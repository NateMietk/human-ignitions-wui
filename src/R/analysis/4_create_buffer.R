
# Buffered FPA perimeters
if (!file.exists(file.path(fire_poly, "fpa_mtbs_bae.gpkg"))) {
  # Create the distance variable to create the simple buffers
  
  fpa_fire <- fpa_fire %>%
    mutate(MTBS_ACRES = NA,
           MTBS_DISCOVERY_YEAR = NA,
           MTBS_DISCOVERY_MONTH = NA,
           MTBS_DISCOVERY_DAY = NA,
           RADIUS = NA) %>%
    dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, MTBS_ACRES, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                  MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH,
                  MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION, RADIUS)
  
  bae <- fpa_fire %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt(FIRE_SIZE_m2/pi)) %>%
    filter(is.na(MTBS_ID))
  
  bae <- st_parallel(bae, st_buffer, n_cores = ncores, dist = bae$RADIUS) %>%
    st_transform(proj_ea) 
  
  bae <- do.call(rbind, list(bae = bae, mtbs_fire = mtbs_fire)) %>%
    st_cast('POLYGON') %>%
    dplyr::select(FPA_ID, DISCOVERY_YEAR, FIRE_SIZE_km2, geometry) 
  
  st_write(bae, file.path(fire_poly, "fpa_mtbs_bae.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  bae <- st_read(file.path(fire_poly, "fpa_mtbs_bae.gpkg"))
}

# Buffered FPA perimeters intersected with the WUI data
if (!file.exists(file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg"))) {
  fpa_df <- as_tibble(as.data.frame(fpa_fire)) %>%
    dplyr::select(-geom)
  
  fpa_bae_wui1 <- bae %>%
    st_intersection(., wui) %>%
    st_intersection(., bounds) %>%
    st_make_valid() %>%
    left_join(., fpa_df, by = 'FPA_ID')
  
  fpa_bae_wui  <- fpa_bae_wui1 %>%
    mutate(wui_area_km2 = (as.numeric(st_area(geom))/1000000),
           class = as.factor(ifelse(DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, as.character(Class90),
                                    ifelse(DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, as.character(Class00),
                                           ifelse(DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, as.character(Class10),
                                                  NA)))),
           ten_year = as.factor(ifelse(DISCOVERY_YEAR >= 1994 & DISCOVERY_YEAR <= 2004, '1994-2004',
                                       ifelse(DISCOVERY_YEAR >= 2005 & DISCOVERY_YEAR <= 2015, '2005-2015', NA))),
           bidecadal = as.factor(ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                                        ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2000, 2000,
                                               ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2005, 2005,
                                                      ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2010, 2010,
                                                             ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2015,
                                                                    DISCOVERY_YEAR )))))),
           decadal = as.factor(ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 2000, 1990,
                                      ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2010, 2000,
                                             ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2010,
                                                    DISCOVERY_YEAR )))),
           FIRE_SIZE_km2 = FIRE_SIZE_km2.x,
           number_of_persons = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POP1990,
                                       ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POP2000,
                                               ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POP2010, NA))),
           pop_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POPDEN1990,
                             ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POPDEN2000,
                                     ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POPDEN2010, NA ))),
           house_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HUDEN1990,
                               ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HUDEN2000,
                                       ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HUDEN2010, NA ))),
           house_units = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HHU1990,
                                 ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HHU2000,
                                         ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HHU2010, NA ))),
           class_coarse =  as.factor(ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                                             ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))),
           seasons = as.factor(classify_seasons(DISCOVERY_DOY)),
           size = as.factor(classify_fire_size_cl(FIRE_SIZE_km2)),
           regions = as.factor(ifelse(regions == 'East', 'North East', as.character(regions)))) %>%
    dplyr::select(-FIRE_SIZE_km2.x, -FIRE_SIZE_km2.y, -stusps.1) %>%
    dplyr::select(-matches('(1990|2000|2010|00|90|s10|flag|wuiclass|veg|blk|water|shape)')) %>%
    setNames(tolower(names(.)))  %>%
    left_join(., wuw, by = c('class', 'class_coarse', 'decadal'))
  
  st_write(fpa_bae_wui, file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
} else {
  fpa_bae_wui <- st_read(file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg")) 
}

# Buffered FPA 250m perimeters 
if (!file.exists(file.path(fire_poly, 'fpa_buffer_250m.gpkg'))) {
  
  fpa_250m <- bae %>%
    st_buffer(., dist = 250) 
  
  st_write(fpa_250m, file.path(fire_poly, "fpa_buffer_250m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  fpa_250m <- st_read(file.path(fire_poly, "fpa_buffer_250m.gpkg"))
}

# Buffer ICS 209 perimeters
if (!file.exists(file.path(fire_poly, "ics209_bae.gpkg"))) {
  # Create the distance variable to create the simple buffers
  
  ics209_bae <- wui_209 %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt(area_km2*1000000/pi))
  
  ics209_bae <- ics209_bae %>%
    st_buffer(., dist = ics209_bae$RADIUS) %>%
    st_transform(proj_ea) %>%
    st_cast('POLYGON')
  
  st_write(ics209_bae, file.path(fire_poly, "ics209_bae.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  ics209_bae <- st_read(file.path(fire_poly, "ics209_bae.gpkg"))
}


# output dataframe for rmarkdown
if(!file.exists(file.path(rmarkdown_files, 'fpa_bae_wui_df.rds'))) {
  as_tibble(as.data.frame(fpa_bae_wui)) %>%
    dplyr::select(-c(latitude, longitude, geom, ics_209_incident_number, ics_209_name, mtbs_id, mtbs_fire_name)) %>%
    write_rds(file.path(rmarkdown_files, 'fpa_bae_wui_df.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
}

# output dataframe for rmarkdown
if(!file.exists(file.path(rmarkdown_files, 'ics209_bae_df.rds'))) {
  as_tibble(as.data.frame(ics209_bae)) %>%
    dplyr::select(-c(geometry)) %>%
    write_rds(file.path(rmarkdown_files, 'ics209_bae_df.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
}
