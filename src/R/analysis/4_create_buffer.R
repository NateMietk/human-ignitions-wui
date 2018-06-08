
# Buffer FPA points based on radius, remove MTBS present in FPA, replace with the actual MTBS polygons
if (!file.exists(file.path(fire_poly, "fpa_mtbs_bae.gpkg"))) {
  # Create the distance variable to create the simple buffers

  fpa_fire <- fpa_fire %>%
    mutate(MTBS_ACRES = NA,
           MTBS_DISCOVERY_YEAR = NA,
           MTBS_DISCOVERY_MONTH = NA,
           MTBS_DISCOVERY_DAY = NA) %>%
    dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, MTBS_ACRES, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                  MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH,
                  MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION)

  bae <- fpa_fire %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt(FIRE_SIZE_m2/pi)) %>%
    filter(is.na(MTBS_ID))

  bae <- st_parallel(bae, st_buffer, n_cores = ncores, dist = bae$RADIUS)

  bae <- rbind(bae, mtbs_fire) %>%
    st_transform(proj_ztrax) %>%
    st_cast('POLYGON') %>%
    left_join(., as.data.frame(fpa_fire), by = 'FPA_ID') %>%
    dplyr::select(FPA_ID, FIRE_SIZE_km2, geometry) 

  st_write(bae, file.path(fire_poly, "fpa_mtbs_bae.gpkg"),
           driver = "GPKG", delete_layer = TRUE)

  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  bae <- st_read(file.path(fire_poly, "fpa_mtbs_bae.gpkg"))
}

# Create the buffered fpa points intersected with the WUI data
if (!file.exists(file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg"))) {
  fpa_df <- as.data.frame(fpa_fire) %>%
    dplyr::select(-geom)
  
  fpa_bae_wui <- bae %>%
    left_join(., fpa_df, by = 'FPA_ID') %>%
    st_intersection(., wui) %>%
    st_intersection(., bounds) %>%
    st_make_valid() %>%
    mutate(wui_area_km2 = (as.numeric(st_area(geom))/1000000),
           class = ifelse(DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, as.character(Class90),
                          ifelse(DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, as.character(Class00),
                                 ifelse(DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, as.character(Class10),
                                        NA))),
           FIRE_SIZE_km2 = FIRE_SIZE_km2.x,
           seasons = classify_seasons(DISCOVERY_DOY),
           ten_year = ifelse(DISCOVERY_YEAR >= 1994 & DISCOVERY_YEAR <= 2004, '1994-2004',
                             ifelse(DISCOVERY_YEAR >= 2005 & DISCOVERY_YEAR <= 2015, '2005-2015', NA)),
           bidecadal = ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                              ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2000, 2000,
                                     ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2005, 2005,
                                            ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2010, 2010,
                                                   ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2015,
                                                          DISCOVERY_YEAR ))))),
           decadal = ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 2000, 1990,
                            ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2010, 2000,
                                   ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2010,
                                          DISCOVERY_YEAR ))),
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
           class_coarse =  ifelse( Class == 'High Urban' | Class == 'Med Urban' | Class == 'Low Urban', 'Urban',
                                   ifelse( Class == 'Intermix WUI' | Class == 'Interface WUI', 'WUI', as.character(Class))),
           size = classify_fire_size_cl(FIRE_SIZE_km2),
           regions = ifelse(regions == 'East', 'North East', as.character(regions))) %>%
    dplyr::select(-FIRE_SIZE_km2.x, -FIRE_SIZE_km2.y, -stusps.1, -Area_km2) %>%
    dplyr::select(-matches('(1990|2000|2010|00|90|s10|flag|wuiclass|veg|blk|water|shape)')) %>%
    setNames(tolower(names(.))) %>%
    left_join(., wuw_area, by = c('class','decadal')) %>%
    left_join(., coarse_wuw_area, by = c('class_coarse','decadal'))
  
  st_write(fpa_bae_wui, file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)
  
  system(paste0("aws s3 sync ",
                fire_crt, " ", s3_fire_prefix))
} else {
  fpa_bae_wui <- st_read(file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg")) 
  
}


if (!file.exists(file.path(fire_poly, 'fpa_buffer_250m.gpkg'))) {
  
  fpa_250m <- bae %>%
    filter(FIRE_SIZE_km2 >= 0.00025) %>%
    st_buffer(., dist = 250) 
  
  st_write(fpa_250m, file.path(fire_poly, "fpa_buffer_250m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", prefix, " ", s3_base))

} else {
  fpa_250m <- st_read(file.path(fire_poly, "fpa_buffer_250m.gpkg"))
}


