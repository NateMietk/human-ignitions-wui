
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
  fpa_bae_wui <- st_intersection(bae, wui) %>%
    st_intersection(., bounds) %>%
    st_make_valid() %>%
    mutate(Area_km2 = (as.numeric(st_area(geom))/1000000),
           Class = ifelse(DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, as.character(Class90),
                          ifelse(DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, as.character(Class00),
                                 ifelse(DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, as.character(Class10),
                                        NA)))) %>%
    st_transform(proj_ztrax) %>%
    st_cast('POLYGON')

  st_write(fpa_bae_wui, file.path(fire_poly, "fpa_mtbs_bae_wui.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)

  system(paste0("aws s3 sync ",
                fire_crt, " ",
                s3_fire_prefix))
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


