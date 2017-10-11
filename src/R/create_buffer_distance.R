
proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" #USA_Contiguous_Equidistant_Conic

# Create the distance variable to create the simple buffers
fpa_bae <- fpa_fire %>%
  filter(DISCOVERY_YEAR >= 2001) %>%
  st_transform(proj_ed) %>% 
  mutate(radius = sqrt(FIRE_SIZE_m2/pi))

# Buffer FPA points based on radius, remove MTBS present in FPA, replace with the actual MTBS polygons
fpa_bae <- fpa_bae %>%
  st_buffer(., dist = fpa_bae$radius) %>%
  st_transform("+init=epsg:2163") %>%
  filter(MTBS_ID == "<NA>") %>%
  st_union(., mtbs_fire) %>%
  st_intersection(., wui) %>%
  st_intersection(., state_eco) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         ClArea_km2 = area_m2/1000000)

# Calculate the distance of each fire point to Urban boundary.
urban_only <- wui_state_eco %>%
  filter(Class == "Urban") %>%
  st_transform(proj_ed) %>% 
  group_by(group) %>%
  summarize()

fpa_fire_dist <- fpa_fire %>%
  st_transform(proj_ed) 

dist_tbl <- mapply(st_distance, st_geometry(fpa_fire_dist), st_geometry(urban_only)) %>%
  as.tibble() %>%
  mutate(dis_to_wui_m = value,
         dis_to_wui_km = (dis_to_wui_m)*0.001,
         FPA_ID = fpa_fire_dist$FPA_ID) %>%
  select(FPA_ID, dis_to_wui_m, dis_to_wui_km)

fpa_wui_dist <- fpa_fire_dist %>%
  right_join(., dist_tbl, by = "FPA_ID")