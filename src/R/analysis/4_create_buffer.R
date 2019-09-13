
# Buffered FPA perimeters
if (!file.exists(file.path(fire_poly, "fpa_mtbs_bae.gpkg"))) {
  # Create the distance variable to create the simple buffers
  
  fpa_fire_add <- fpa_fire %>%
    mutate(MTBS_ACRES = NA_real_,
           MTBS_DISCOVERY_YEAR = NA_integer_,
           MTBS_DISCOVERY_MONTH = NA_integer_,
           MTBS_DISCOVERY_DAY = NA_integer_,
           GEOMAC_ID = factor(NA_character_),
           GEOMAC_FIRE_NAME = factor(NA_character_),
           GEOMAC_DISCOVERY_YEAR = NA_integer_,
           GEOMAC_ACRES = NA_real_,
           RADIUS = NA_real_) %>%
    dplyr::select(FPA_ID, MTBS_ID, GEOMAC_ID, 
                  GEOMAC_FIRE_NAME, MTBS_FIRE_NAME,
                  FIRE_SIZE, FIRE_SIZE_ha, FIRE_SIZE_km2, MTBS_ACRES, GEOMAC_ACRES, FIRE_SIZE_CL, 
                  DISCOVERY_YEAR, MTBS_DISCOVERY_YEAR, GEOMAC_DISCOVERY_YEAR,
                  MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH, 
                  DISCOVERY_DAY, MTBS_DISCOVERY_DAY, DISCOVERY_DOY,
                  SEASONS, STATE, STAT_CAUSE_DESCR, IGNITION, RADIUS) 
  
  bae_pre <- fpa_fire_add %>%
    anti_join(., as.data.frame(fpa_mtbs_geomac) %>% dplyr::select(FPA_ID)) %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt((FIRE_SIZE*4046.86)/pi)) %>%
    filter(is.na(MTBS_ID)|is.na(GEOMAC_ID))
  
  bae_pre <- st_parallel(bae_pre, st_buffer, n_cores = 1, dist = bae_pre$RADIUS) %>%
    st_transform(proj_ea)
  
  names(bae_pre)[25] = "geom"
  st_geometry(bae_pre) = "geom"
  
  stopifnot(identical(names(bae_pre), names(fpa_mtbs_geomac)))
  
  bae <- do.call(rbind, list(bae_pre = bae_pre, fpa_mtbs_geomac = fpa_mtbs_geomac)) %>%
    st_cast('MULTIPOLYGON') %>% 
    st_make_valid()
  rownames(bae) <- NULL
  st_write(bae, file.path(fire_poly, "fpa_mtbs_geomac_bae.gpkg"))
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  bae <- st_read(file.path(fire_poly, "fpa_mtbs_bae.gpkg")) %>%
    st_set_precision(., precision = 0.001)
}

# Buffered FPA 250m perimeters
if (!file.exists(file.path(fire_poly, 'fpa_buffer_250m.gpkg'))) {
  fpa_250m <- st_parallel(bae, st_buffer, n_cores = parallel::detectCores(), dist = 250) %>%
    st_set_precision(., precision = 0.001)
  
  fpa_250m_slim <- fpa_250m %>% 
    dplyr::select(FPA_ID)
  bae_slim <- bae %>% 
    dplyr::select(FPA_ID)
  
  fpa_250m <- st_erase(fpa_250m_slim, y = bae_slim)
  
  st_write(fpa_250m, file.path(fire_poly, "fpa_buffer_250m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  fpa_250m <- st_read(file.path(fire_poly, "fpa_buffer_250m.gpkg"))
}

# Buffered FPA 500m perimeters
if (!file.exists(file.path(fire_poly, 'fpa_buffer_500m.gpkg'))) {
  rm(bae)
  fpa_500m <- st_parallel(fpa_250m, st_buffer, n_cores = parallel::detectCores(), dist = fpa_250m) %>%
    st_set_precision(., precision = 0.001)
  fpa_500m <- st_parallel(fpa_500m, st_erase, n_cores = parallel::detectCores(), y = fpa_250m)
  
  st_write(fpa_500m, file.path(fire_poly, "fpa_buffer_500m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  fpa_500m <- st_read(file.path(fire_poly, "fpa_buffer_500m.gpkg")) 
}

# Buffered FPA 1000m perimeters
if (!file.exists(file.path(fire_poly, 'fpa_buffer_1000m.gpkg'))) {
  rm(fpa_250m)
  fpa_1000m <- fpa_500m %>%
    st_buffer(., dist = 500)
  # fpa_1000m <- st_parallel(fpa_1000m, st_erase, n_cores = parallel::detectCores(), y = fpa_500m)
  
  st_write(fpa_1000m, file.path(fire_poly, "fpa_buffer_1000m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  fpa_1000m <- st_read(file.path(fire_poly, "fpa_buffer_1000m.gpkg")) 
}

# Buffered FPA perimeters intersected with the WUI data
if (!file.exists(file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg"))) {
  fpa_df <- as_tibble(as.data.frame(fpa_fire)) %>%
    dplyr::select(-contains('geom|geometry'))
  
  fpa_bae_wui1 <- bae %>%
    st_intersection(., wui) %>%
    st_make_valid() 
  
  fpa_bae_wui <- fpa_bae_wui1 %>%
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
           size = as.factor(classify_fire_size(FIRE_SIZE_km2)),
           regions = as.factor(ifelse(regions == 'East', 'North East', as.character(regions)))) %>%
    rename_all(tolower) %>%
    dplyr::select(-stusps.1) %>%
    dplyr::select(-matches('(1990|2000|2010|00|90|s10|flag|wuiclass|veg|blk|water|shape)'))
  
  st_write(fpa_bae_wui, file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
} else {
  fpa_bae_wui <- st_read(file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg")) 
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

# Buffer ICS 209 250m perimeters
if (!file.exists(file.path(fire_poly, "ics209_bae_250m.gpkg"))) {
  # Create the distance variable to create the simple buffers
  
  ics209_bae_250m <- ics209_bae %>%
    st_buffer(., dist = 250) 
  
  st_write(ics209_bae_250m, file.path(fire_poly, "ics209_bae_250m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  ics209_bae_250m <- st_read(file.path(fire_poly, "ics209_bae_250m.gpkg"))
}

# Buffer ICS 209 500m perimeters
if (!file.exists(file.path(fire_poly, "ics209_bae_500m.gpkg"))) {
  # Create the distance variable to create the simple buffers
  
  ics209_bae_500m <- ics209_bae %>%
    st_buffer(., dist = 500) 
  
  st_write(ics209_bae_500m, file.path(fire_poly, "ics209_bae_500m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  ics209_bae_500m <- st_read(file.path(fire_poly, "ics209_bae_500m.gpkg"))
}

# Buffer ICS 209 1000m perimeters
if (!file.exists(file.path(fire_poly, "ics209_bae_1000m.gpkg"))) {
  # Create the distance variable to create the simple buffers
  
  ics209_bae_1000m <- ics209_bae %>%
    st_buffer(., dist = 1000) 
  
  st_write(ics209_bae_1000m, file.path(fire_poly, "ics209_bae_1000m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  ics209_bae_1000m <- st_read(file.path(fire_poly, "ics209_bae_1000m.gpkg"))
}

# output dataframe for rmarkdown
if(!file.exists(file.path(rmarkdown_files, 'fpa_bae_wui_df.rds'))) {
  fpa_bae_wui_df <- as_tibble(as.data.frame(fpa_bae_wui))
  write_rds(fpa_bae_wui_df, file.path(rmarkdown_files, 'fpa_bae_wui_df.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
} else {
  fpa_bae_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_bae_wui_df.rds'))
}

if(!file.exists(file.path(rmarkdown_files, 'ics209_bae_df.rds'))) {
  ics209_bae_df <- as_tibble(as.data.frame(ics209_bae)) %>%
    dplyr::select(-c(geom)) %>%
    write_rds(file.path(rmarkdown_files, 'ics209_bae_df.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
} else {
  ics209_bae_df <- read_rds(file.path(rmarkdown_files, 'ics209_bae_df.rds'))
}

if(!file.exists(file.path(rmarkdown_files, 'ics209_bae_250m_df.rds'))) {
  ics209_bae_250m_df <- as_tibble(as.data.frame(ics209_bae_250m)) %>%
    dplyr::select(-c(geom)) %>%
    write_rds(file.path(rmarkdown_files, 'ics209_bae_250m_df.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
} else {
  ics209_bae_250m_df <- read_rds(file.path(rmarkdown_files, 'ics209_bae_250m_df.rds'))
}
