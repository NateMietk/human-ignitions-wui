#Clean and prep the MTBS data to match the FPA database naming convention
if (!exists('mtbs_fire')) {
  if (!file.exists(file.path(mtbs_out, "mtbs_conus_2000_2015.gpkg"))) {
    mtbs_fire <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                         layer = "dissolve_mtbs_perims_1984-2015_DD_20170501", quiet= TRUE) %>%
      filter(Year >= 2000) %>%
      st_transform(st_crs(usa_shp)) %>%
      mutate(MTBS_ID = Fire_ID,
             MTBS_FIRE_NAME = Fire_Name,
             DISCOVERY_YEAR = Year,
             DISCOVERY_DAY = StartDay,
             DISCOVERY_MONTH = StartMonth,
             ACRES = Acres,
             RADIUS = "NA",
             FPA_ID = NA) %>%
      dplyr::select(FPA_ID, MTBS_ID, MTBS_FIRE_NAME, DISCOVERY_DAY, DISCOVERY_MONTH, DISCOVERY_YEAR, ACRES, RADIUS) %>%
      st_make_valid()
    
    st_write(mtbs_fire, file.path(mtbs_out, "mtbs_conus_2000_2015.gpkg"), driver = "GPKG")

  } else {
    mtbs_fire <- st_read(dsn = file.path(mtbs_out, "mtbs_conus_2000_2015.gpkg"))
  }
}

# Buffered FPA perimeters
if (!file.exists(file.path(fire_poly, "fpa_mtbs_bae_2000_2015.gpkg"))) {
  # Create the distance variable to create the simple buffers
  
  fpa_fire_mtbs_compatible <- fpa_fire %>%
    filter(DISCOVERY_YEAR >= 2000) %>%
    mutate(ACRES = FIRE_SIZE,
           RADIUS = NA) %>%
    dplyr::select(FPA_ID, MTBS_ID, MTBS_FIRE_NAME, DISCOVERY_DAY, DISCOVERY_MONTH, DISCOVERY_YEAR, ACRES, RADIUS)
  
  bae_mtbs_compatible <- fpa_fire_mtbs_compatible %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt(ACRES*4046.86/pi)) %>% # radius in m2
    filter(is.na(MTBS_ID))
  
  bae_mtbs_compatible <- st_parallel(bae_mtbs_compatible, st_buffer, n_cores = ncores, dist = bae_mtbs_compatible$RADIUS) %>%
    st_transform(proj_ea)
  
  bae_mtbs_compatible <- do.call(rbind, list(bae_mtbs_compatible = bae_mtbs_compatible, 
                                             mtbs_fire = mtbs_fire)) %>%
    st_cast('POLYGON') %>%
    dplyr::select(FPA_ID, MTBS_ID, DISCOVERY_YEAR, ACRES, geometry)
  
  st_write(bae_mtbs_compatible, file.path(fire_poly, "fpa_mtbs_bae_2000_2015.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
} else {
  bae_mtbs_compatible <- st_read(file.path(fire_poly, "fpa_mtbs_bae_2000_2015.gpkg")) 
}

totals <- as.data.frame(bae_mtbs_compatible) %>%
  group_by() %>%
  summarise(frequency = n(),
            burned_area = sum(ACRES)) %>% 
  as_tibble()

# How many MTBS polygons are present?
mtbs_present <- as.data.frame(bae_mtbs_compatible) %>%
  filter(!is.na(MTBS_ID)) %>%
  group_by() %>%
  summarise(mtbs_frequency = n(),
            mtbs_burned_area = sum(ACRES),
            pct_mtbs_freq = mtbs_frequency/totals$frequency*100,
            pct_mtbs_burned = mtbs_burned_area/totals$burned_area*100) %>% 
  as_tibble()
  