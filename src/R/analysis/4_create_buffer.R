
# Buffer FPA points based on radius, remove MTBS present in FPA, replace with the actual MTBS polygons
if (!file.exists(file.path(fpa_out, "fpa_mtbs_bae.gpkg"))) {
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

  bae <- st_parallel(bae, st_buffer, n_cores = ncores, dist = bae$RADIUS) %>%
    st_transform(st_crs(usa_shp))

  bae <- rbind(bae, mtbs_fire)

  st_write(bae, file.path(fpa_out, "fpa_mtbs_bae.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)

    system(paste0("aws s3 sync ",
                fire_crt, " ",
                s3_fire_prefix))
} else {
  bae <- st_read(file.path(fpa_out, "fpa_mtbs_bae.gpkg"))
}

if (!file.exists(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"))) {
  fpa_bae_wui <- st_par(bae, st_intersection, n_cores = ncores, y = wui) %>%
    st_par(., st_intersection, n_cores = ncores, y = bounds) %>%
    st_par(., st_make_valid, n_cores = ncores) %>%
    mutate(Area_km2 = (as.numeric(st_area(geom))/1000000))

  st_write(fpa_bae_wui, file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)
} else {
  fpa_bae_wui <- st_read(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"))

  }
