
geo_mac <- st_read(file.path(geo_mac_raw_dir, 'US_HIST_FIRE_PERIMTRS_DD83.gdb')) %>%
  st_transform(st_crs(usa_shp)) %>%
  filter(FIRE_YEAR > '2015')


geo_mac_fpa <- geo_mac %>%
  st_intersection(., fpa_fire)

tmp <- geo_mac_fpa %>%
  dplyr::select(fire_name, FPA_ID, ICS_209_INCIDENT_NUMBER, year_, FIRE_YEAR, acres, FIRE_SIZE) %>%
  filter(year_ == FIRE_YEAR) %>%
  filter(acres <= FIRE_SIZE+(FIRE_SIZE*0.25) & acres >= FIRE_SIZE-(FIRE_SIZE*0.25))


fire_repeats <- as.data.frame(tmp) %>%
  group_by(fire_name) %>%
  count() %>%
  arrange(desc(n))

temmper <-   tmp %>%
  left_join(., fire_repeats, by = 'fire_name') %>%
  filter(n <= 1)
