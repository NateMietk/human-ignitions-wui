proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" #USA_Contiguous_Equidistant_Conic
ncores <- detectCores()

# Create the distance variable to create the simple buffers
bae <- fpa_fire %>%
  st_transform(proj_ed) %>%
  mutate(RADIUS = sqrt(FIRE_SIZE_m2/pi)) %>%
  filter(is.na(MTBS_ID))

# Buffer FPA points based on radius, remove MTBS present in FPA, replace with the actual MTBS polygons
bae <- st_par(bae, st_buffer, n_cores = ncores, dist = bae$RADIUS) %>%
  st_transform("+init=epsg:2163")

fire_list <- list(bae, mtbs_fire)
bae <- do.call(rbind, fire_list)

if (!file.exists(file.path(fpa_out, "fpa_mtbs_bae.gpkg"))) {
  st_write(bae, file.path(fpa_out, "fpa_mtbs_bae.gpkg"),
           driver = "GPKG",
           update=TRUE)}

fpa_bae <- st_par(bae, st_intersection, n_cores = ncores, y = wui) %>%
  st_par(., st_intersection, n_cores = ncores, y = state_eco_fish) %>%
  st_par(., st_make_valid, n_cores = ncores) %>%
  mutate(Area_km2 = (as.numeric(st_area(geom))/1000000))

if (!file.exists(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"))) {
  st_write(fpa_bae, file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"),
           driver = "GPKG",
           update=TRUE)}
