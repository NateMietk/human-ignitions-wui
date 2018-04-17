
# Buffer FPA points based on radius, remove MTBS present in FPA, replace with the actual MTBS polygons
if (!file.exists(file.path(fpa_out, "fpa_mtbs_bae.gpkg"))) {
  # Create the distance variable to create the simple buffers
  bae <- fpa_fire %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt(FIRE_SIZE_m2/pi)) %>%
    filter(is.na(MTBS_ID))

  bae <- st_par(bae, st_buffer, n_cores = ncores, dist = bae$RADIUS) %>%
    st_transform("+init=epsg:2163")

  fire_list <- list(bae, mtbs_fire)
  bae <- do.call(rbind, fire_list)

  st_write(bae, file.path(fpa_out, "fpa_mtbs_bae.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)
} else {
  bae <- st_read(file.path(fpa_out, "fpa_mtbs_bae.gpkg"))
}

if (!file.exists(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"))) {
  fpa_bae_wui <- st_par(bae, st_intersection, n_cores = ncores, y = wui) %>%
    st_par(., st_intersection, n_cores = ncores, y = state_eco_fish) %>%
    st_par(., st_make_valid, n_cores = ncores) %>%
    mutate(Area_km2 = (as.numeric(st_area(geom))/1000000))

  st_write(fpa_bae_wui, file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)
} else {
  fpa_bae_wui <- st_read(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"))

  }
